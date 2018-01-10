(*)
type vertex
type weight
type edge
type graph
*)

type t
type vertex

type pipeline = vertex list

val vertex_opt : vertex -> Optf.optimization

val create_graph : unit -> t

val add_edge : t -> vertex -> vertex -> t

val dump_graph : t -> unit

val create_vertex : Optf.optimization -> Verify.state -> Cost.cost -> vertex
val remove_vertex : t -> vertex -> unit

val get_opt : vertex -> Optf.optimization
val get_state : vertex -> Verify.state
val get_cost : vertex -> Cost.cost

val get_pred : t -> vertex -> vertex

val traverse : t -> vertex -> unit

val get_pipelines : t -> vertex list-> vertex -> pipeline list

val generate_pipeline : t -> vertex -> pipeline