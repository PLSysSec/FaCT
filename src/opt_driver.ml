open Lwt

let sleep_time = 1.

(* This queue contains all of the optimizations. This is used to add an
   optimization to the pipelines. Optimizations are only removed from this
   queue -- never added. *)
let opt_queue = ref (Queue.create ())
(* Optimizations are added to the swap queue *)
let opt_queue_swap = ref (Queue.create ())

(* This queue contains the worklist of vertexes in the graph to add
  optimizations to. *)
let vertex_queue = Queue.create ()

(* Initialize the optimization queue *)
let _ = List.iter (fun o -> Queue.add o !opt_queue) Optf.opts

(* Initialize the vertex queue. This consists of creating the graph and adding
  an edge between the noopt vertex and each optimization *)
let opt_graph = Graphf.create_graph ()
let noopt = Graphf.create_vertex Optf.NoOptimization Verify.Secure Cost.Unknown
(* Add all of the optimizations to the graph and the vertexes to the worklist *)
let _ = List.iter (fun opt -> 
                    let v = Graphf.create_vertex opt Verify.Unknown Cost.Unknown in
                    Queue.add v vertex_queue;
                    Graphf.add_edge opt_graph noopt v |> ignore)
                  Optf.opts

let graph_mutex = Lwt_mutex.create ()

(* The number of times to steal work from the worklist *)
let num_iterations = ref 0
let continue = ref true

let print_errors errors =
  Hashtbl.iter
  (fun (des,det) pass ->
    Log.error "%s" 
      (Verify.show_checkerstatus (Verify.Error(pass,des,det))))
  errors

(*
  DRIVER CODE
*)

(*
  1. Get next vertex from the worklist
    1. Generate the pipeline for the vertex
    2. Verify the pipeline
    3. While queue is not empty
      1. Get the next optimization from the queue
      1. Add the optimization as a new vertex connected to the current vertex
      2. Add the new vertex to the end worklist
    4. Swap old and new queues

*)

exception Done

let print_pipeline p =
  let p' = List.map (fun v -> Optf.show_optimization(Graphf.vertex_opt(v))) p in
  String.concat " -> " p'

let run_pipeline pipeline opt_name llmod =
  let llmod' = Llvm_transform_utils.clone_module llmod in
  let pm = Llvm.PassManager.create () in
  let opts = List.map (fun v -> Graphf.vertex_opt v) pipeline in
  List.iter (fun opt ->
    match Optf.to_llopt opt with
      | Some (opt,name) -> (*Log.error "Added %s" name; *)opt pm
      | None -> ()) opts;
  (*if (List.length opts) < 5 then Cost.Unknown, Verify.Unknown else begin*)
  Log.error "Running with size `%d` and vertex `%s`" (List.length opts) opt_name;
  match Llvm.PassManager.run_module llmod' pm with
    | false ->
      let cost = Cost.generate_cost llmod' in
      cost, Verify.Unchanged
    | true -> 
      let errors = Hashtbl.create 100 in
      let cost = Cost.generate_cost llmod' in
      let a = begin match Verify.verify errors opt_name llmod' with
        | Verify.InSecure -> (*print_errors errors;*) Verify.InSecure
        | Verify.Secure -> Log.debug "\n\n\n\n\n\nYAYYYYYYY SECURE!!!!!!\n\n\n\n\n\n"; Verify.Secure
        | a -> a end in
    cost, a

let rec drive llmod =
  (* Connect vertex to each element in opt_queue *)
  let rec process_opt_queue vertex =
    let next = try Some (Queue.take !opt_queue) with Queue.Empty -> None in
    match next with
      | None -> Lwt.return_unit
      | Some opt ->
        Queue.add opt !opt_queue_swap;
        let current_opt = Graphf.get_opt vertex in
        (* We dont want to do the same opt twice in a row *)
        match current_opt == opt with
          | true -> process_opt_queue vertex
          | false ->
            let vertex' = Graphf.create_vertex opt Verify.Unknown Cost.Unknown in
            Graphf.add_edge opt_graph vertex vertex' |> ignore;
            Queue.add vertex' vertex_queue;
            process_opt_queue vertex in
  let update_vertex vertex state cost =
    let opt = Graphf.get_opt vertex in
    let vertex' = Graphf.create_vertex opt state cost in
    let pred_vertex = Graphf.get_pred opt_graph vertex in
    Graphf.add_edge opt_graph pred_vertex vertex' |> ignore;
    Graphf.remove_vertex opt_graph vertex;
    vertex' in
  let run vertex pipeline opt_name llmod =
    (* If a bug was introduced, then (for now) we dont want to continue down
       this pipeline. Instead, we want to focus on other parts of the graph
       that are *more likely* to be secure *)
    match run_pipeline pipeline opt_name llmod with
      | cost,Verify.InSecure -> (* Replace the vertex *)
        Log.error "Insecure";
        update_vertex vertex Verify.InSecure cost |> ignore;
        drive llmod
        (*let vertex = update_vertex vertex Verify.InSecure cost in
        process_opt_queue vertex |> ignore;
        (* Swap the queues *)
        let swap = !opt_queue in
        opt_queue := !opt_queue_swap;
        opt_queue_swap := swap;
        (* Recurse! *)
        drive llmod;*)
      | cost,Verify.Secure ->
        Log.error "\n\n\n\nSecure\n\n\n\n";
        let vertex = update_vertex vertex Verify.Unchanged cost in
        process_opt_queue vertex |> ignore;
        (* Swap the queues *)
        let swap = !opt_queue in
        opt_queue := !opt_queue_swap;
        opt_queue_swap := swap;
        (* Recurse! *)
        drive llmod
      | cost,Verify.Unchanged ->
        (* Here we problably want to update the queue to move this vertext to
          the end, or find why this was unchanged and de-prioritize it in the
          future *)
        Log.error "Unchanged";
        update_vertex vertex Verify.Unchanged cost |> ignore;
        drive llmod
      | cost, Verify.Unknown ->
        let vertex = update_vertex vertex Verify.Unknown cost in
        process_opt_queue vertex |> ignore;
        (* Swap the queues *)
        let swap = !opt_queue in
        opt_queue := !opt_queue_swap;
        opt_queue_swap := swap;
        (* Recurse! *)
        drive llmod in
  match !num_iterations with
    | n ->
      if n mod 100 == 0 then Log.error "Graph size: %d" (Graphf.size opt_graph);
      num_iterations := !num_iterations + 1;
      if not !continue then Lwt.return_unit else
      (* Vertex to operate on *)
      let vertex = try Some(Queue.take vertex_queue) with | Queue.Empty -> None in
      match vertex with
        | None -> Log.info "Pipeline generation has completed!"; Lwt.return_unit
        | Some v ->
          let opt_name = Optf.show_optimization (Graphf.get_opt v) in
          let pipeline = Graphf.generate_pipeline opt_graph v in
          run v pipeline opt_name llmod




(*
  END DRIVER CODE
*)

let pick_pipeline () =
  let v = ref None in
  let apply vertex =
    match Graphf.get_state vertex with
      | Verify.Secure -> v := Some vertex
      | _ -> () in
  Graphf.bfs apply opt_graph;
  match !v with
    | None -> Log.error "No optmization pipeline found"
    | Some v ->
      Log.error "Fuckin right. Found a pipeline! %s" (Optf.show_optimization (Graphf.get_opt v))
      


(*
  Current state:
    The optimizer can now build the graph for essentially eternity to find the
    right pipeline.
    When a new pipeline is generated, it will calculate the security and cost
    of it and store it in the graph.

  Next:
    1. Do not continue building a pipeline if the state becomes insecure
    2. Do not continue a pipeline if the cost does not change?
        - This could backfire. It could be setting up an optimization for big gains
    3. Do not continue a pipeline if the module does not change

*)


(*
    Start pair pruning methodology
*)

(*
    1. Generate pairs
      - this is each optimization paired with all the other optimizations in every order
    2. Send each pair through the optimization pipeline
    3. Compare each pairs counterpart and pick one pair to go to the next round
    4. Repeat until only one pair exists
*)

type opt_group =  Optf.optimization list 

type result =
  | LHS
  | RHS
  | EQUAL

type pair =
  | Pair of opt_group * opt_group

type compared =
  | Compared of opt_group * opt_group * result

let normalize_list l =
  let rec normalize_list' l acc =
    match l with
      | [] -> acc
      | f::r -> normalize_list' r ([f]::acc) in
  normalize_list' l []

let generate_initial_pairs opts =
  let rec get_pairs current l (acc : pair list) =
    match l with
      | [] -> acc
      | f::r -> 
        let pair = Pair(current @ f, f @ current) in
        get_pairs current r (pair::acc) in
  let rec get_all_pairs l (acc : pair list) =
    match l with
      | [] -> acc
      | f::r -> get_all_pairs r (get_pairs f r acc) in
  get_all_pairs opts []

let generate_new_pairs (pairs : compared list) =
  (*
    1. Convert list of compareds to list of opt_groups
    2. Generate pairs with opt_groups
  *)
  let to_opt_group = function
    | Compared (og,og',LHS) -> og
    | Compared (og,og',RHS) -> og'
    | Compared (og,og',EQUAL) -> og in
  let opt_groups = List.map to_opt_group pairs in
  generate_initial_pairs opt_groups

let dummy_compare pairs =
  let compare = function
    | Pair(og,og') ->
      begin match Random.int 3 with
        | 0 -> Compared(og,og',LHS)
        | 1 -> Compared(og,og',RHS)
        | _ -> Compared(og,og',EQUAL) end in
  List.map compare pairs

let pair_prune llmod =
  let opts = normalize_list Optf.opts in
  let pairs = generate_initial_pairs opts in
  let group_string (g : opt_group) = String.concat " -> " (List.map Optf.show_optimization g) in
  let pair_string = function
    | Pair (g1,g2) -> "Group1: " ^ (group_string g1) ^ "\nGroup2: " ^ (group_string g2) in
  List.iter (fun g -> Log.error "%s" (pair_string g)) pairs;

  Log.error "\n\n\n\n\n\n\n\n\n\n\n\n\n";

  (*)
  let compared_pairs = dummy_compare pairs in
  let pairs' = generate_new_pairs compared_pairs in
  (*List.iter (fun g -> Log.error "%s" (pair_string g)) pairs';*)
  Log.error "%d vs %d" (List.length pairs) (List.length pairs');*)

  let run_pipeline' (og : opt_group) llmod =
    let llmod' = Llvm_transform_utils.clone_module llmod in
    let pm = Llvm.PassManager.create () in
    List.iter (fun opt ->
    match Optf.to_llopt opt with
      | Some (opt,name) -> (*Log.error "Added %s" name; *)opt pm
      | None -> ()) og;
    match Llvm.PassManager.run_module llmod' pm with
      | false ->
        (*let cost = Cost.generate_cost llmod' in*)
        5, Verify.Unchanged
      | true -> 
        (*let cost = Cost.generate_cost llmod' in*)
        let errors = Hashtbl.create 100 in
        let opt_name = String.concat ", " (List.map Optf.show_optimization og) in
        let state = begin match Verify.verify errors opt_name llmod' with
          | Verify.Secure as s-> Log.error "Found something secure: %s" opt_name; s
          | s -> s end in
        5, state
    in

  let count = ref 0 in

  let run_pair secure_pipelines = function
    | Pair(og,og') ->
      Log.error "Count: %d" !count;
      count := !count + 1;
      let cost,state   = run_pipeline' og llmod in
      let cost',state' = run_pipeline' og' llmod in
      let actual_state, new_og = if cost > cost' then state,og else state',og' in
      match actual_state with
        | Verify.Secure ->
          let opt_name = String.concat ", " (List.map Optf.show_optimization og) in
          Log.error "Added secure pipeline? %s" opt_name;
          new_og::secure_pipelines
        | _ -> secure_pipelines in
  
  let secure = List.fold_left run_pair [] pairs in
  Log.error "Found %d secure pipelines!" (List.length secure);
  List.iter (fun g -> Log.error "Pipeline: %s" (group_string g)) secure;
  ()


(*
    End pair pruning methodology
*)