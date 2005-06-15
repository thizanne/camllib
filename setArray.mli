(*
 *      $Id: setArray.mli,v 1.3 2003/11/12 20:39:02 bjeannet Exp $
 *
 *      Ensembles implantes avec des tableaux tries
 *)

(** Sets over totally ordered type with arrays *)

type 'a t
val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val empty : 'a t
val is_empty : 'a t -> bool
val mem : 'a -> 'a t -> bool
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val of_array : 'a array -> 'a t
val to_array : 'a t -> 'a array
val singleton : 'a -> 'a t
val add : 'a -> 'a t -> 'a t
val remove : 'a -> 'a t -> 'a t
val union : 'a t -> 'a t -> 'a t
val inter : 'a t -> 'a t -> 'a t
val diff : 'a t -> 'a t -> 'a t
val compare : 'a t -> 'a t -> int
val equal : 'a t -> 'a t -> bool
val subset : 'a t -> 'a t -> bool
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val cardinal : 'a t -> int
val elements : 'a t -> 'a list
val min_elt : 'a t -> 'a
val max_elt : 'a t -> 'a
val choose : 'a t -> 'a
val filter : ('a -> bool) -> 'a t -> 'a t
val partition : ('a -> bool) -> 'a t -> ('a t * 'a t)

(** Output signature of the functor {!SetArray.Make} *)
module type S =
  sig
    type elt
    type t
    val print : (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val of_list : elt list -> t
    val to_list : t -> elt list
    val of_array : elt array -> t
    val to_array : t -> elt array
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
    val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> (t * t)
  end

(** Functor building an implementation of the SetArray structure
   given a totally ordered type. *)
module Make(Ord: Set.OrderedType): (S with type elt = Ord.t)
