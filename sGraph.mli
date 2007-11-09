(** Directed graphs, functional API, with one-way information maintained, *)

(*  ********************************************************************** *)
(** {2 Polymorphic version} *)
(*  ********************************************************************** *)

type ('a,'b,'c) t

val succ : ('a,'b,'c) t -> 'a -> 'a Sette.t
val pred : ('a,'b,'c) t -> 'a -> 'a Sette.t
val etiq : ('a,'b,'c) t -> 'a -> 'b
val arete : ('a,'b,'c) t -> 'a * 'a -> 'c
val empty : ('a,'b,'c) t
val size : ('a,'b,'c) t -> int
val is_empty : ('a,'b,'c) t -> bool
val is_vertex : ('a,'b,'c) t -> 'a -> bool
val is_edge : ('a,'b,'c) t -> 'a * 'a -> bool
val vertices : ('a,'b,'c) t -> 'a Sette.t
val edges : ('a,'b,'c) t -> ('a * 'a) Sette.t

val map_vertex : ('a,'b,'c) t -> ('a -> 'b -> 'd) -> ('a, 'd, 'c) t
val map_edge : ('a,'b,'c) t -> ('a * 'a -> 'c -> 'd) -> ('a, 'b, 'd) t
val iter_vertex : ('a,'b,'c) t -> ('a -> 'b -> 'a Sette.t -> unit) -> unit
val iter_edge : ('a,'b,'c) t -> (('a * 'a) -> 'c -> unit) -> unit
val fold_vertex : ('a,'b,'c) t -> 'd -> ('a -> 'b -> 'a Sette.t -> 'd -> 'd) -> 'd
val fold_edge : ('a,'b,'c) t -> 'd -> ('a * 'a -> 'c -> 'd -> 'd) -> 'd

val add_edge : ('a,'b,'c) t -> 'a * 'a -> 'c -> ('a,'b,'c) t
val remove_edge : ('a,'b,'c) t -> 'a * 'a -> ('a,'b,'c) t
val add_vertex : ('a,'b,'c) t -> 'a -> 'b -> ('a,'b,'c) t
val remove_vertex : ('a,'b,'c) t -> 'a -> ('a,'b,'c) t
val topological_sort : ('a,'b,'c) t -> 'a -> 'a list
val topological_sort_multi : 'a -> ('a,'b,'c) t -> 'a Sette.t -> 'a list
val accessible : ('a,'b,'c) t -> 'a -> 'a Sette.t
val accessible_multi :
  'a -> ('a,'b,'c) t -> 'a Sette.t -> 'a Sette.t
val cfc : ('a,'b,'c) t -> 'a -> 'a list list
val cfc_multi : 'a -> ('a,'b,'c) t -> 'a Sette.t -> 'a list list
val scfc : ('a,'b,'c) t -> 'a -> 'a Ilist.t
val scfc_multi : 'a -> ('a,'b,'c) t -> 'a Sette.t -> 'a Ilist.t
val min : ('a,'b,'c) t -> 'a Sette.t
val max : ('a,'b,'c) t -> 'a Sette.t
val print : (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) -> (Format.formatter -> 'c -> unit) -> Format.formatter -> ('a,'b,'c) t -> unit
(*  ********************************************************************** *)
(** {2 Functor version} *)
(*  ********************************************************************** *)

module type T = sig
  module MapV : Mappe.S
    (** Map module for associating attributes to vertices, of type [MapV.key]
      *)
  module MapE : (Mappe.S with type key = MapV.key * MapV.key)
    (** Map module for associating attributes to edges, of type [MapV.key *
      MapV.key] *)
end

module type S = sig
  type vertex
    (** The type of vertices *)
  module SetV : (Sette.S with type elt=vertex)
    (** The type of sets of vertices *)
  module SetE : (Sette.S with type elt=vertex*vertex)
    (** The type of sets of edges *)
  module MapV : (Mappe.S with type key=vertex and module Setkey=SetV)
    (** The Map for vertices *)
  module MapE : (Mappe.S with type key=vertex*vertex and module Setkey=SetE)
    (** The Map for edges *)
	    
  type ('b,'c) t
    (** The type of graphs, where:
      - ['b] is the type of vertex attribute (etiq);
      - ['c] is the type of edge attributes (arete)
    *)
    
  val succ : ('b,'c) t -> vertex -> SetV.t
  val pred : ('b,'c) t -> vertex -> SetV.t
  val etiq : ('b,'c) t -> vertex -> 'b
  val arete : ('b,'c) t -> vertex * vertex -> 'c
  val empty : ('b,'c) t
  val size : ('b,'c) t -> int
  val is_empty : ('b,'c) t -> bool
  val is_vertex : ('b,'c) t -> vertex -> bool
  val is_edge : ('b,'c) t -> vertex * vertex -> bool
  val vertices : ('b,'c) t -> SetV.t
  val edges : ('b,'c) t -> SetE.t

  val map_vertex : ('b,'c) t -> (vertex -> 'b -> 'd) -> ('d, 'c) t
  val map_edge : ('b,'c) t -> (vertex * vertex -> 'c -> 'd) -> ('b, 'd) t
  val iter_vertex : ('b,'c) t -> (vertex -> 'b -> SetV.t -> unit) -> unit
  val iter_edge : ('b,'c) t -> ((vertex * vertex) -> 'c -> unit) -> unit
  val fold_vertex : ('b,'c) t -> 'd -> (vertex -> 'b -> SetV.t -> 'd -> 'd) -> 'd
  val fold_edge : ('b,'c) t -> 'd -> (vertex * vertex -> 'c -> 'd -> 'd) -> 'd

  val add_edge : ('b,'c) t -> vertex * vertex -> 'c -> ('b,'c) t
  val remove_edge : ('b,'c) t -> vertex * vertex -> ('b,'c) t
  val add_vertex : ('b,'c) t -> vertex -> 'b -> ('b,'c) t
  val remove_vertex : ('b,'c) t -> vertex -> ('b,'c) t
  val topological_sort : ('b,'c) t -> vertex -> vertex list
  val topological_sort_multi : vertex -> ('b,'c) t -> SetV.t -> vertex list
  val accessible : ('b,'c) t -> vertex -> SetV.t
  val accessible_multi :
  vertex -> ('b,'c) t -> SetV.t -> SetV.t
  val cfc : ('b,'c) t -> vertex -> vertex list list
  val cfc_multi : vertex -> ('b,'c) t -> SetV.t -> vertex list list
  val scfc : ('b,'c) t -> vertex -> vertex Ilist.t
  val scfc_multi : vertex -> ('b,'c) t -> SetV.t -> vertex Ilist.t
  val min : ('b,'c) t -> SetV.t
  val max : ('b,'c) t -> SetV.t
  val print : (Format.formatter -> vertex -> unit) -> (Format.formatter -> 'b -> unit) -> (Format.formatter -> 'c -> unit) -> Format.formatter -> ('b,'c) t -> unit
end

module Make(T : T) : (S with type vertex=T.MapV.key
			and module SetV=T.MapV.Setkey
			and module SetE=T.MapE.Setkey
			and module MapV=T.MapV
			and module MapE=T.MapE)
