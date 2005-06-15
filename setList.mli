(*
        setList.mli

        Ensembles implantes avec des listes triees
*)

(*
 *      $Log: setList.mli,v $
 *      Revision 1.5  2003/11/12 20:39:02  bjeannet
 *      OK for ocamldoc.
 *
 *      Revision 1.4  2003/11/12 17:26:00  bjeannet
 *      Mappe and Sette changed according to the OCaml 3.07 standard library.
 *      Printing functions generalized with optional arguments.
 *      Miscellaneous.
 *
 *      Revision 1.3  2003/02/12 09:56:38  bjeannet
 *      Added functions exists and for_all.
 *
 *      Revision 1.2  2001/07/12 15:37:26  bjeannet
 *      Corrected the type of SetList.fold, to be conforming to Set.fold.
 *
 *      Revision 1.1.1.1  2001/01/25 08:44:10  bjeannet
 *      Library of Caml modules dealing with sets, multisets, graphs, nested
 *      lists (Ilist), additionnal functions to standard module list (Listc),
 *      and a polymorphic version of standard module Map.
 *
 *
 *      Revision 1.1  1998/12/04 16:11:13  bjeannet
 *      Initial revision
 *
 *)

(** Sets over totally ordered type with lists *)

type 'a t
val print : 
  ?first:(unit, Format.formatter, unit) format ->
  ?sep:(unit, Format.formatter, unit) format ->
  ?last:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val empty : 'a t
val is_empty : 'a t -> bool
val mem : 'a -> 'a t -> bool
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
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
val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val cardinal : 'a t -> int
val elements : 'a t -> 'a list
val min_elt : 'a t -> 'a
val max_elt : 'a t -> 'a
val choose : 'a t -> 'a
val filter : ('a -> bool) -> 'a t -> 'a t
val partition : ('a -> bool) -> 'a t -> ('a t * 'a t)
val exists : ('a -> bool) -> 'a t -> bool
val for_all : ('a -> bool) -> 'a t -> bool

(** Output signature of the functor {!SetList.Make} *)
module type S =
  sig
    type elt
    type t
    val print : 
      ?first:(unit, Format.formatter, unit) format ->
      ?sep:(unit, Format.formatter, unit) format ->
      ?last:(unit, Format.formatter, unit) format ->
      (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val of_list : elt list -> t
    val to_list : t -> elt list
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> (t * t)
    val exists : (elt -> bool) -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
  end

(** Functor building an implementation of the SetList structure
   given a totally ordered type. *)
module Make(Ord: Set.OrderedType): (S with type elt = Ord.t)
