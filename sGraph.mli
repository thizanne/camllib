(*
 * $Id: sGraph.mli,v 1.2 2003/11/12 20:39:02 bjeannet Exp $
 *
 * Operations on directed graphs (one-way information maintained)
 *
 *)

(** Directed graphs, with one-way information maintained *)

type ('a, 'b, 'c) t

val succ : ('a, 'b, 'c) t -> 'a -> 'a Sette.t
val pred : ('a, 'b, 'c) t -> 'a -> 'a Sette.t
val etiq : ('a, 'b, 'c) t -> 'a -> 'b
val arete : ('a, 'b, 'c) t -> 'a * 'a -> 'c
val empty : ('a, 'b, 'c) t
val size : ('a, 'b, 'c) t -> int
val is_empty : ('a, 'b, 'c) t -> bool
val is_vertex : ('a, 'b, 'c) t -> 'a -> bool
val is_edge : ('a, 'b, 'c) t -> 'a * 'a -> bool
val vertices : ('a, 'b, 'c) t -> 'a Sette.t
val edges : ('a, 'b, 'c) t -> ('a * 'a) Sette.t

val map_vertex : ('a, 'b, 'c) t -> ('a -> 'b -> 'd) -> ('a, 'd, 'c) t
val map_edge : ('a, 'b, 'c) t -> ('a * 'a -> 'c -> 'd) -> ('a, 'b, 'd) t
val iter_vertex : ('a,'b,'c) t -> ('a -> 'b -> 'a Sette.t -> unit) -> unit
val iter_edge : ('a,'b,'c) t -> (('a * 'a) -> 'c -> unit) -> unit
val fold_vertex : ('a, 'b, 'c) t -> 'd -> ('a -> 'b -> 'a Sette.t -> 'd -> 'd) -> 'd
val fold_edge : ('a, 'b, 'c) t -> 'd -> ('a * 'a -> 'c -> 'd -> 'd) -> 'd

val add_edge : ('a, 'b, 'c) t -> 'a * 'a -> 'c -> ('a, 'b, 'c) t
val remove_edge : ('a, 'b, 'c) t -> 'a * 'a -> ('a, 'b, 'c) t
val add_vertex : ('a, 'b, 'c) t -> 'a -> 'b -> ('a, 'b, 'c) t
val remove_vertex : ('a, 'b, 'c) t -> 'a -> ('a, 'b, 'c) t
val topological_sort : ('a, 'b, 'c) t -> 'a -> 'a list
val topological_sort_multi : 'a -> ('a, 'b, 'c) t -> 'a Sette.t -> 'a list
val accessible : ('a, 'b, 'c) t -> 'a -> 'a Sette.t
val accessible_multi :
  'a -> ('a, 'b, 'c) t -> 'a Sette.t -> 'a Sette.t
val cfc : ('a, 'b, 'c) t -> 'a -> 'a list list
val cfc_multi : 'a -> ('a, 'b, 'c) t -> 'a Sette.t -> 'a list list
val scfc : ('a, 'b, 'c) t -> 'a -> 'a Ilist.t
val scfc_multi : 'a -> ('a, 'b, 'c) t -> 'a Sette.t -> 'a Ilist.t
val min : ('a, 'b, 'c) t -> 'a Sette.t
val max : ('a, 'b, 'c) t -> 'a Sette.t
val print : (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) -> (Format.formatter -> 'c -> unit) -> Format.formatter -> ('a,'b,'c) t -> unit
