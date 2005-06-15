(*
        multiSetList.mli

	Multi-ensembles avec des listes

 *
 *      $log$
 *
 *)

(** Multisets implemented with lists *)

type 'a t
val of_set : 'a SetList.t -> 'a t
val to_set : 'a t -> 'a SetList.t
val empty : 'a t
val is_empty : 'a t -> bool
val mem : 'a -> 'a t -> bool
val mult : 'a -> 'a t -> int
val singleton : 'a * int -> 'a t
val add : 'a * int -> 'a t -> 'a t
val remove : 'a * int -> 'a t -> 'a t
val union : 'a t -> 'a t -> 'a t
val inter : 'a t -> 'a t -> 'a t
val diff : 'a t -> 'a t -> 'a t
val union_set : 'a t -> 'a SetList.t -> 'a t
val inter_set : 'a t -> 'a SetList.t -> 'a t
val diff_set : 'a t -> 'a SetList.t -> 'a t
val compare : 'a t -> 'a t -> int
val equal : 'a t -> 'a t -> bool
val subset : 'a t -> 'a t -> bool
val iter : (('a * int) -> unit) -> 'a t -> unit
val fold : (('a * int) -> 'b -> 'b) -> 'a t -> 'b -> 'b
val fold_right : (('a * int) -> 'b -> 'b) -> 'a t -> 'b -> 'b
val fold_left : ('b -> ('a * int) -> 'b) -> 'b -> 'a t -> 'b
val cardinal : 'a t -> int
val elements : 'a t -> 'a SetList.t
val min_elt : 'a t -> 'a
val max_elt : 'a t -> 'a
val min : 'a t -> 'a * int
val max : 'a t -> 'a * int
val mins : 'a t -> 'a SetList.t * int
val maxs : 'a t -> 'a SetList.t * int
val choose : 'a t -> 'a
val print :  
  ?first:(unit, Format.formatter, unit) format ->
  ?sep:(unit, Format.formatter, unit) format ->
  ?last:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit

(** Output signature of the functor {!MultiSetList.Make} *)
module type S =
  sig
    type elt
    type t
    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val mult: elt -> t -> int
    val add: elt * int -> t -> t
    val singleton: elt * int -> t
    val remove: elt * int -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val union_set : t -> elt SetList.t -> t
    val inter_set: t -> elt SetList.t -> t
    val diff_set: t -> elt SetList.t -> t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val subset: t -> t -> bool
    val iter: ((elt * int) -> unit) -> t -> unit
    val fold: ((elt * int) -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_right: ((elt * int) -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_left: ('a -> (elt * int) -> 'a) -> 'a -> t -> 'a
    val cardinal: t -> int
    val elements: t -> elt SetList.t
    val min_elt: t -> elt
    val max_elt: t -> elt
    val min: t -> elt * int
    val max: t -> elt * int
    val mins: t -> elt SetList.t * int
    val maxs: t -> elt SetList.t * int
    val choose: t -> elt
    val of_set: elt SetList.t -> t
    val to_set: t -> elt SetList.t
    val print: 
      ?first:(unit, Format.formatter, unit) format ->
      ?sep:(unit, Format.formatter, unit) format ->
      ?last:(unit, Format.formatter, unit) format ->
      (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit

  end

(** Functor building an implementation of the MultiSetList structure
   given a totally ordered type. *)
module Make(Ord: Set.OrderedType) : (S with type elt = Ord.t)
