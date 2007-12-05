(** Directed graphs, functional API, with two-way information maintained, *)

(*  ********************************************************************** *)
(** {2 Polymorphic version} *)
(*  ********************************************************************** *)

type ('a,'b,'c,'d) t

val info : ('a,'b,'c,'d) t -> 'd
val set_info : ('a,'b,'c,'d) t -> 'd -> ('a,'b,'c,'d) t 

val succ : ('a,'b,'c,'d) t -> 'a -> 'a Sette.t
val pred : ('a,'b,'c,'d) t -> 'a -> 'a Sette.t
val attrvertex : ('a,'b,'c,'d) t -> 'a -> 'b
val attredge : ('a,'b,'c,'d) t -> 'a * 'a -> 'c
val empty : 'd -> ('a,'b,'c,'d) t
val size : ('a,'b,'c,'d) t -> int
val is_empty : ('a,'b,'c,'d) t -> bool
val is_vertex : ('a,'b,'c,'d) t -> 'a -> bool
val is_edge : ('a,'b,'c,'d) t -> 'a * 'a -> bool
val vertices : ('a,'b,'c,'d) t -> 'a Sette.t
val edges : ('a,'b,'c,'d) t -> ('a * 'a) Sette.t

val map_vertex : ('a,'b,'c,'d) t -> ('a -> 'b -> pred:'a Sette.t -> succ:'a Sette.t -> 'e) -> ('a, 'e, 'c, 'd) t
val map_edge : ('a,'b,'c,'d) t -> ('a * 'a -> 'c -> 'e) -> ('a, 'b, 'e, 'd) t
val map : 
  ('a,'b,'c,'d) t ->
  ('a -> 'b -> pred:'a Sette.t -> succ:'a Sette.t -> 'bb) ->
  ('a * 'a -> 'c -> 'cc) ->
  ('d -> 'dd) ->
  ('a,'bb,'cc,'dd) t

val transpose : 
  ('a,'b,'c,'d) t ->
  ('a -> 'b -> pred:'a Sette.t -> succ:'a Sette.t -> 'bb) ->
  ('a * 'a -> 'c -> 'cc) ->
  ('d -> 'dd) ->
  ('a,'bb,'cc,'dd) t

val iter_vertex : ('a,'b,'c,'d) t -> ('a -> 'b -> pred:'a Sette.t -> succ:'a Sette.t -> unit) -> unit
val iter_edge : ('a,'b,'c,'d) t -> (('a * 'a) -> 'c -> unit) -> unit
val fold_vertex : ('a,'b,'c,'d) t -> 'e -> ('a -> 'b -> pred:'a Sette.t -> succ:'a Sette.t -> 'e -> 'e) -> 'e
val fold_edge : ('a,'b,'c,'d) t -> 'e -> ('a * 'a -> 'c -> 'e -> 'e) -> 'e

val add_edge : ('a,'b,'c,'d) t -> 'a * 'a -> 'c -> ('a,'b,'c,'d) t
val remove_edge : ('a,'b,'c,'d) t -> 'a * 'a -> ('a,'b,'c,'d) t
val add_vertex : ('a,'b,'c,'d) t -> 'a -> 'b -> ('a,'b,'c,'d) t
val remove_vertex : ('a,'b,'c,'d) t -> 'a -> ('a,'b,'c,'d) t
val topological_sort : ('a,'b,'c,'d) t -> 'a -> 'a list
val topological_sort_multi : 'a -> ('a,'b,'c,'d) t -> 'a Sette.t -> 'a list
val reachable : ('a,'b,'c,'d) t -> 'a -> 'a Sette.t
val reachable_multi :
  'a -> ('a,'b,'c,'d) t -> 'a Sette.t -> 'a Sette.t
val cfc : ('a,'b,'c,'d) t -> 'a -> 'a list list
val cfc_multi : 'a -> ('a,'b,'c,'d) t -> 'a Sette.t -> 'a list list
val scfc : ('a,'b,'c,'d) t -> 'a -> 'a Ilist.t
val scfc_multi : 'a -> ('a,'b,'c,'d) t -> 'a Sette.t -> 'a Ilist.t
val min : ('a,'b,'c,'d) t -> 'a Sette.t
val max : ('a,'b,'c,'d) t -> 'a Sette.t
val print : 
  (Format.formatter -> 'a -> unit) -> 
  (Format.formatter -> 'b -> unit) -> 
  (Format.formatter -> 'c -> unit) -> 
  (Format.formatter -> 'd -> unit) -> 
   Format.formatter -> ('a,'b,'c,'d) t -> unit

type ('a,'b) nodezz = {
    succzz: 'a Sette.t;
    predzz: 'a Sette.t;
    attrvertexzz: 'b
  }
type ('a,'b,'c,'d) tzz = {
    nodeszz: ('a, ('a,'b) nodezz) Mappe.t;
    arcszz: ('a*'a,'c) Mappe.t;
    infozz: 'd;
  }
val repr : ('a,'b,'c,'d) t -> ('a,'b,'c,'d)  tzz
val obj : ('a,'b,'c,'d) tzz -> ('a,'b,'c,'d) t

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
	    
  type ('b,'c,'d) t
    (** The type of graphs, where:
      - ['b] is the type of vertex attribute (attrvertex);
      - ['c] is the type of edge attributes (attredge)
    *)
    
  val info : ('b,'c,'d) t -> 'd
  val set_info : ('b,'c,'d) t -> 'd -> ('b,'c,'d) t 
  val succ : ('b,'c,'d) t -> vertex -> SetV.t
  val pred : ('b,'c,'d) t -> vertex -> SetV.t
  val attrvertex : ('b,'c,'d) t -> vertex -> 'b
  val attredge : ('b,'c,'d) t -> vertex * vertex -> 'c
  val empty : 'd -> ('b,'c,'d) t
  val size : ('b,'c,'d) t -> int
  val is_empty : ('b,'c,'d) t -> bool
  val is_vertex : ('b,'c,'d) t -> vertex -> bool
  val is_edge : ('b,'c,'d) t -> vertex * vertex -> bool
  val vertices : ('b,'c,'d) t -> SetV.t
  val edges : ('b,'c,'d) t -> SetE.t

  val map_vertex : ('b,'c,'d) t -> (vertex -> 'b -> pred:SetV.t -> succ:SetV.t -> 'e) -> ('e, 'c,'d) t
  val map_edge : ('b,'c,'d) t -> (vertex * vertex -> 'c -> 'e) -> ('b, 'e, 'd) t
  val map :
    ('b,'c,'d) t ->
    (vertex -> 'b -> pred:SetV.t -> succ:SetV.t -> 'bb) ->
    (vertex * vertex -> 'c -> 'cc) ->
    ('d -> 'dd) ->
    ('bb,'cc,'dd) t
  val transpose : 
    ('b,'c,'d) t ->
    (vertex -> 'b -> pred:SetV.t -> succ:SetV.t -> 'bb) ->
    (vertex * vertex -> 'c -> 'cc) ->
    ('d -> 'dd) ->
    ('bb,'cc,'dd) t
  val iter_vertex : ('b,'c,'d) t -> (vertex -> 'b -> pred:SetV.t -> succ:SetV.t -> unit) -> unit
  val iter_edge : ('b,'c,'d) t -> ((vertex * vertex) -> 'c -> unit) -> unit
  val fold_vertex : ('b,'c,'d) t -> 'e -> (vertex -> 'b -> pred:SetV.t -> succ:SetV.t -> 'e -> 'e) -> 'e
  val fold_edge : ('b,'c,'d) t -> 'e -> (vertex * vertex -> 'c -> 'e -> 'e) -> 'e
  val add_edge : ('b,'c,'d) t -> vertex * vertex -> 'c -> ('b,'c,'d) t
  val remove_edge : ('b,'c,'d) t -> vertex * vertex -> ('b,'c,'d) t
  val add_vertex : ('b,'c,'d) t -> vertex -> 'b -> ('b,'c,'d) t
  val remove_vertex : ('b,'c,'d) t -> vertex -> ('b,'c,'d) t
  val topological_sort : ('b,'c,'d) t -> vertex -> vertex list
  val topological_sort_multi : vertex -> ('b,'c,'d) t -> SetV.t -> vertex list
  val reachable : ('b,'c,'d) t -> vertex -> SetV.t
  val reachable_multi :
  vertex -> ('b,'c,'d) t -> SetV.t -> SetV.t
  val cfc : ('b,'c,'d) t -> vertex -> vertex list list
  val cfc_multi : vertex -> ('b,'c,'d) t -> SetV.t -> vertex list list
  val scfc : ('b,'c,'d) t -> vertex -> vertex Ilist.t
  val scfc_multi : vertex -> ('b,'c,'d) t -> SetV.t -> vertex Ilist.t
  val min : ('b,'c,'d) t -> SetV.t
  val max : ('b,'c,'d) t -> SetV.t
  val print :
    (Format.formatter -> vertex -> unit) -> 
    (Format.formatter -> 'b -> unit) -> 
    (Format.formatter -> 'c -> unit) -> 
    (Format.formatter -> 'd -> unit) -> 
    Format.formatter -> ('b,'c,'d) t -> unit

  type 'b nodezz = {
    succzz: SetV.t;
    predzz: SetV.t;
    attrvertexzz: 'b
  }
  type ('b,'c,'d) tzz = {
    nodeszz: 'b nodezz MapV.t;
    arcszz: 'c MapE.t;
    infozz : 'd;
  }

  val repr : ('b,'c,'d) t -> ('b,'c,'d)  tzz
  val obj : ('b,'c,'d) tzz -> ('b,'c,'d) t
end

module Make(T : T) : (S with type vertex=T.MapV.key
			and module SetV=T.MapV.Setkey
			and module SetE=T.MapE.Setkey
			and module MapV=T.MapV
			and module MapE=T.MapE)
