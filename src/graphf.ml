
exception VertexInvariantError
exception NoPredError

module Vertex = struct
  type t = Optf.optimization * Verify.state * Cost.cost
end

module Edge = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end

module G = Graph.Imperative.Digraph.AbstractLabeled(Vertex)(Edge)

type t = G.t
type vertex = G.vertex
type pipeline = vertex list

let vertex_opt v = let o,_,_ = G.V.label v in o

let index = ref 50

let next_index () =
  let ret = !index in
  index := ret + 100;
  ret

let create_graph () = G.create ()

let create_vertex opt state cost = G.V.create (opt, state, cost)

let remove_vertex g v = G.remove_vertex g v

let get_opt v =
  let opt,_,_ = G.V.label v in opt

let get_state v =
  let _,s,_ = G.V.label v in s

let get_cost v =
  let _,_,c = G.V.label v in c

let add_edge g f t = G.add_edge g f t; g

let get_pred g v =
  match G.pred g v with
    | [] -> raise NoPredError
    | [p] -> p
    | f::r -> raise VertexInvariantError

let traverse g v =
  let print_vertex v =
    let (o,s,c) = G.V.label v in
    Log.error "%s %s" (Optf.show_optimization o) (Verify.show_state s) in
  G.iter_succ print_vertex g v;
  ()

let rec get_pipelines g cp v =
  let current_pipeline = v::cp in
  let apply cp next acc =
    let np = next::cp in
    np::acc in
  let apply' = apply current_pipeline in
  let r = G.fold_succ apply' g v [] in
  let worklist = List.map List.hd r in
  let next_level = List.map (get_pipelines g current_pipeline) worklist in
  let f = List.flatten next_level in
  List.fold_left (fun acc l -> l::acc) r f

(* This only works if each vertex only has 1 predecessor *)
let generate_pipeline g v =
  let rec generate_pipeline' v acc =
    match G.pred g v with
      | [] -> acc
      | [v'] -> generate_pipeline' v' (v'::acc)
      | f::r -> raise VertexInvariantError in
  generate_pipeline' v [v]
  (*G.fold_pred (fun v' acc -> v'::acc) g v [v]*)

let dump_graph g =
  G.iter_edges (fun v1 v2 ->
    let x1, y1, c1 = G.V.label v1 in
    let x2, y2, c2 = G.V.label v2 in
    let x1, x2 = (Optf.show_optimization x1),(Optf.show_optimization x2) in
    Format.printf "%s,%s@\n" x1 x2) g;
  Format.printf "@?"