(* $Id: graph.mli,v 1.5 2005/06/14 14:19:56 bjeannet Exp $ *)

(** Operations on directed graphs. (Two-way information maintained) *)

(** The semantics of the operations is functional. *)

type ('a,'b,'c) t
  (** The type of graphs, where:
    - ['a] is the type of vertex;
    - ['b] is the type of vertex attribute (etiq);
    - ['c] is the type of edge attributes (arete)
  *)

val succ : ('a, 'b, 'c) t -> 'a -> 'a Sette.t
  (** @return the set of successor vertices of the given vertex in the given
    graph. *)
val pred : ('a, 'b, 'c) t -> 'a -> 'a Sette.t
  (** @return the set of predecessor vertices of the given vertex in the given
 graph. *)
val etiq : ('a, 'b, 'c) t -> 'a -> 'b
  (** @return the attribute associated to the given vertex in the given graph
    @raise Not_found if the vertex does not exist *)
val arete : ('a, 'b, 'c) t -> 'a * 'a -> 'c
  (** @return the attribute associated to the given edge in the given graph 
    @raise Not_found if the edge does not exist *)
val empty : ('a, 'b, 'c) t
  (** The empty graph *)
val size : ('a, 'b, 'c) t -> int * int
  (** @return [(v,e)], resp. the number of vertices and the number of edges in
    the graph. *)
val is_empty : ('a, 'b, 'c) t -> bool
  (** Is the graph empty ? *)
val is_vertex : ('a, 'b, 'c) t -> 'a -> bool
  (** Does the vertex belong to the graph ? *)
val is_edge : ('a, 'b, 'c) t -> 'a * 'a -> bool
  (** Does the edge belong to the graph ? *)
val vertices : ('a, 'b, 'c) t -> 'a Sette.t
  (** @return the set of vertices *)
val edges : ('a, 'b, 'c) t -> ('a * 'a) Sette.t
  (** @return the set of edges *)
val add_edge : ('a, 'b, 'c) t -> 'a * 'a -> 'c -> ('a, 'b, 'c) t
  (** [add_edge graph (org,dest) arc] adds a new edge [(org,dest)] to
    the graph and associates to it the attribute [arc]. 
    @return the new graph
    @raise Failure if [org] or [dest] are not vertices of [graph] 
  *)
val remove_edge : ('a, 'b, 'c) t -> 'a * 'a -> ('a, 'b, 'c) t
  (** [remove_edge graph (org,dest)] removes the edge [(org,dest)] from the
    graph.  @return the new graph 

    @raise Failure if [org] or [dest] are not vertices of [graph] *)
val add_vertex : ('a, 'b, 'c) t -> 'a -> 'b -> ('a, 'b, 'c) t
  (** [add_vertex graph vertex attr] adds a new vertex to the graph and
    associates to it the attribute [attr]. If the vertex already exists, it
    just changes its attribute.  
    @return the new graph *)
val remove_vertex : ('a, 'b, 'c) t -> 'a -> ('a, 'b, 'c) t
  (** [remove_vertex graph vertex] removes the vertex from the graph.
    @return the new graph
    @raise Failure if [vertex] is not a vertex of the graph
  *)
val duplicate_vertex : ('a, 'b, 'c) t -> 'a -> 'a -> 'b -> ('a, 'b, 'c) t
  (** Vertex duplication.  [duplicate_vertex graph vertex newvertex newattr]
    creates a new vertex [newvertex] with the attribute [newattr] and
    duplicates all the transitions involving [vertex], included a possible
    self-loop.  
    @return the new graph 
    @raise Failure if [vertex] is not a vertex of the graph *)
val transpose : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Transpose the given graph (inverse the direction of edges).
    @return the transposed graph
  *)
val topological_sort : ('a, 'b, 'c) t -> 'a -> 'a list
  (** Topological sort from a single vertex. 
    [topological_sort graph root] performs a topological sort of the
    vertices of the graph that are reachable from [root]. The acyclicity of
    the graph is not checked, and if it does contain cycles, the result is
    meaningless.
    @return a increasing linear order 
    @raise Not_found if [root] is not a vertex of the graph *)
val topological_sort_multi : 'a -> ('a, 'b, 'c) t -> 'a Sette.t -> 'a list
  (** Topological sort from several vertices. 
    [topological_sort_multi root graph sroot] performs a topological sort of
    the vertices of the graph that are reachable from any (existing) vertex of
    the set [sroot]. [root] is a dummy vertex which is assumed not to belong
    to the graph. The acyclicity of the graph is not checked, and if it does
    contain cycles, the result is meaningless.
    @return a increasing linear order *)
val accessible : ('a, 'b, 'c) t -> 'a -> 'a Sette.t
  (** Reachability from a single vertex.
    @return the set of vertices reachable from the given vertex
    @raise Not_found if the vertex does not belong to the graph *)
val accessible_multi :
  'a -> ('a, 'b, 'c) t -> 'a Sette.t -> 'a Sette.t
  (** Reachability from several vertices.  [accessible_multi root graph sroot]
    returns the set of vertices that are reachable from any vertex of the set
    [sroot]. [root] is a dummy vertex which is assumed not to belong
    to the graph.
    @return the set of vertices reachable from [sroot]
  *)
val coaccessible : ('a, 'b, 'c) t -> 'a -> 'a Sette.t
  (** Dual operations of [accessible] *)
val coaccessible_multi : 'a -> ('a, 'b, 'c) t -> 'a Sette.t -> 'a Sette.t
  (** Dual operations of [accessible_multi] *)
val cfc : ('a, 'b, 'c) t -> 'a -> 'a list list
  (** Strongly-Connected-Component decomposition, from a single vertex.  [cfc
    graph root] decomposes the subgraph reachable from [root] into strongly
    connected components. Each component is defined by a list of vertices.
    @return the strongly connected components sorted by inccreasing topological
    order
    @raise Not_found if the vertex does not belong to the graph *)
val cfc_multi : 'a -> ('a, 'b, 'c) t -> 'a Sette.t -> 'a list list
  (** Strongly-Connected-Component decomposition, from several vertices.
    [cfc_multi root graph sroot] decomposes the subgraph reachable from the set
    [sroot] into strongly connected components. Each component is defined by a
    list of vertices.  [root] is a dummy vertex which is assumed not to belong
    to the graph.

    @return the strongly connected components sorted by increasing topological
    order *)
val cfc_filter_multi : 'a -> ('a -> 'a -> bool) -> ('a, 'b, 'c) t -> 'a Sette.t -> 'a list list
  (** Strongly-Connected-Component decomposition, from several vertices, but
    with set of edges restricted by the predicate function. *)
val scfc : ('a, 'b, 'c) t -> 'a -> 'a Ilist.t
  (** Strongly-Connected-Sub-Component decomposition, from a single vertex. *)
val scfc_multi : 'a -> ('a, 'b, 'c) t -> 'a Sette.t -> 'a Ilist.t
  (** Strongly-Connected-Sub-Component decomposition, from several vertices. *)
val scfc_filter_multi : 'a -> ('a -> 'a -> bool) -> ('a, 'b, 'c) t -> 'a Sette.t -> 'a Ilist.t
  (** Strongly-Connected-Sub-Component decomposition, from several vertices,
    but with set of edges restricted by the predicate function. *)
val min : ('a, 'b, 'c) t -> 'a Sette.t
  (** @return the set of vertices without predecessors *)
val max : ('a, 'b, 'c) t -> 'a Sette.t
  (** @return the set of vertices without successors *)
val print : (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) -> (Format.formatter -> 'c -> unit) -> Format.formatter -> ('a,'b,'c) t -> unit
  (** Prints the graph, given printing functions for vertices and vertex and
    edge attributes. *)
val print_dot :
    ?titlestyle:string ->
    ?vertexstyle:string ->
    ?edgestyle:string ->
    ?title:string ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'a -> 'b -> unit) ->
    (Format.formatter -> ('a * 'a) -> 'c -> unit) ->
    Format.formatter -> ('a, 'b, 'c) t -> unit
  (** Prints the graph to DOT format. *)

val map_vertex : ('a, 'b, 'c) t -> ('a -> 'b -> 'd) -> ('a, 'd, 'c) t
  (** Map function on vertices and their attributes.

    @return a new grah, where attributes [attr] associated to vertices [vertex]
    have been replaced by [f vertex attr] *)
val map_edge : ('a, 'b, 'c) t -> ('a * 'a -> 'c -> 'd) -> ('a, 'b, 'd) t
  (** Map function on edges and their attributes.

    @return a new grah, where attributes [arc] associated to edges [(org,dest)]
    have been replaced by [f (org,dest) arc] *)
val map : ('a,'b,'c) t -> 
  ('a -> 'b -> 'd) ->
  ('a * 'a -> 'c -> 'e) ->
  ('a,'d,'e) t
  (** Perform in the same time [map_vertex] and [map_edge] on the graph. *)
val iter_vertex : ('a,'b,'c) t -> ('a -> 'b -> unit) -> unit
  (** Iteration on vertices and their attributes *)
val iter_edge : ('a,'b,'c) t -> (('a * 'a) -> 'c -> unit) -> unit
  (** Iteration on edges and their attributes *)
val fold_vertex : ('a, 'b, 'c) t -> 'd -> ('a -> 'b -> 'd -> 'd) -> 'd
  (** Iteration with accumulation of a result on vertices and their attributes
    *)
val fold_edge : ('a, 'b, 'c) t -> 'd -> ('a * 'a -> 'c -> 'd -> 'd) -> 'd
  (** Iteration with accumulation of a result on edges and their attributes *)

module type Param = 
sig
    module MapV : Mappe.S
	(* Map module for associating attributes to vertices, of type [MapV.key] *)
    module MapE : (Mappe.S with type key = MapV.key * MapV.key)
	(* Map module for associating attributes to edges, of type [MapV.key * MapV.key] *)
  end
(** Input signature for the functor {!Graph.Make} *)

module type S =
  sig
    type vertex
	  (* The type of vertices *)
    module SetV : (Sette.S with type elt=vertex)
	(* The type of sets of vertices *)
    module MapV : (Mappe.S with type key=vertex and module Setkey=SetV)
	(* The Map for vertices *)
    module MapE : (Mappe.S with type key=vertex*vertex)
	(* The Map for edges *)
	
    type ('b,'c) t
	  (* The type of graphs, where:
	     - ['b] is the type of vertex attribute (etiq);
	     - ['c] is the type of edge attributes (arete)
	   *)

    val succ : ('b, 'c) t -> vertex -> SetV.t
    val pred : ('b, 'c) t -> vertex -> SetV.t
    val etiq : ('b, 'c) t -> vertex -> 'b
    val arete : ('b, 'c) t -> vertex * vertex -> 'c
    val empty : ('b, 'c) t
    val size : ('b, 'c) t -> int * int
    val is_empty : ('b, 'c) t -> bool
    val is_vertex : ('b, 'c) t -> vertex -> bool
    val is_edge : ('b, 'c) t -> vertex * vertex -> bool
    val vertices : ('b, 'c) t -> SetV.t
    val edges : ('b, 'c) t -> MapE.Setkey.t
    val add_edge : ('b, 'c) t -> vertex * vertex -> 'c -> ('b, 'c) t
    val remove_edge : ('b, 'c) t -> vertex * vertex -> ('b, 'c) t
    val add_vertex : ('b, 'c) t -> vertex -> 'b -> ('b, 'c) t
    val remove_vertex : ('b, 'c) t -> vertex -> ('b, 'c) t
    val duplicate_vertex : ('b, 'c) t -> vertex -> vertex -> 'b -> ('b, 'c) t
    val transpose : ('b, 'c) t -> ('b, 'c) t
    val topological_sort : ('b, 'c) t -> vertex -> vertex list
    val topological_sort_multi : vertex -> ('b, 'c) t -> SetV.t -> vertex list
    val accessible : ('b, 'c) t -> vertex -> SetV.t
    val accessible_multi :
	vertex -> ('b, 'c) t -> SetV.t -> SetV.t
    val coaccessible : ('b, 'c) t -> vertex -> SetV.t
    val coaccessible_multi :
	vertex -> ('b, 'c) t -> SetV.t -> SetV.t
    val cfc : ('b, 'c) t -> vertex -> vertex list list
    val cfc_multi : vertex -> ('b, 'c) t -> SetV.t -> vertex list list
    val cfc_filter_multi : vertex -> (vertex -> vertex -> bool) -> ('b, 'c) t -> SetV.t -> vertex list list
    val scfc : ('b, 'c) t -> vertex -> vertex Ilist.t
    val scfc_multi : vertex -> ('b, 'c) t -> SetV.t -> vertex Ilist.t
    val scfc_filter_multi : vertex -> (vertex -> vertex -> bool) -> ('b, 'c) t -> SetV.t -> vertex Ilist.t
    val min : ('b, 'c) t -> SetV.t
    val max : ('b, 'c) t -> SetV.t
    val print : (Format.formatter -> vertex -> unit) -> (Format.formatter -> 'b -> unit) -> (Format.formatter -> 'c -> unit) -> Format.formatter -> ('b,'c) t -> unit
    val print_dot :
      ?titlestyle:string ->
      ?vertexstyle:string ->
      ?edgestyle:string ->
      ?title:string ->
      (Format.formatter -> vertex -> unit) ->
      (Format.formatter -> vertex -> 'b -> unit) ->
      (Format.formatter -> vertex*vertex -> 'c -> unit) ->
      Format.formatter -> ('b, 'c) t -> unit
    val map_vertex : ('b, 'c) t -> (vertex -> 'b -> 'd) -> ('d, 'c) t
    val map_edge : ('b, 'c) t -> (vertex * vertex -> 'c -> 'd) -> ('b, 'd) t
    val map : 
	('b,'c) t -> 
	  (vertex -> 'b -> 'd) ->
	    (vertex * vertex -> 'c -> 'e) ->
	      ('d,'e) t
		
    val iter_vertex : ('b,'c) t -> (vertex -> 'b -> unit) -> unit
    val iter_edge : ('b,'c) t -> ((vertex * vertex) -> 'c -> unit) -> unit
    val fold_vertex : ('b, 'c) t -> 'd -> (vertex -> 'b -> 'd -> 'd) -> 'd
    val fold_edge : ('b, 'c) t -> 'd -> (vertex * vertex -> 'c -> 'd -> 'd) -> 'd
  end
(** Output signature for the functor {!Graph.Make} *)

module Make(P : Param) : (S with type vertex=P.MapV.key
			    and module SetV=P.MapV.Setkey
			    and module MapV=P.MapV 
			    and module MapE=P.MapE)
(** Functor building an implementation of the [Graph] structure. *)
