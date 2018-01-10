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
                    let v = Graphf.create_vertex opt Verify.Secure Cost.Unknown in
                    Queue.add v vertex_queue;
                    Graphf.add_edge opt_graph noopt v |> ignore)
                  Optf.opts

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
      | Some (opt,name) -> Log.error "Added %s" name; opt pm
      | None -> ()) opts;
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
            let vertex' = Graphf.create_vertex opt Verify.Secure Cost.Unknown in
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
        let vertex = update_vertex vertex Verify.InSecure cost in
        process_opt_queue vertex |> ignore;
        (* Swap the queues *)
        let swap = !opt_queue in
        opt_queue := !opt_queue_swap;
        opt_queue_swap := swap;
        (* Recurse! *)
        drive llmod;
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
        drive llmod in
  match !num_iterations with
    | n ->
      if n mod 100 == 0 then Log.error "Step: %d" n;
      num_iterations := !num_iterations + 1;
      if not !continue then (Log.error "No bueno"; Lwt.return_unit) else
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