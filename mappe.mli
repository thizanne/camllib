(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: mappe.mli,v 1.11 2005/06/14 14:24:07 bjeannet Exp $ *)

(** Association tables over ordered types *)

(** This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map. *)

(** Modification par B. Jeannet pour avoir des mappes génériques *)

type ('a,'b) tzz =
    Emptyzz
  | Nodezz of ('a,'b) tzz * 'a * 'b * ('a,'b) tzz * int

type ('a,'b) t
      (** The type of maps from type ['a] to type ['b]. *)

val repr : ('a,'b) t -> ('a,'b) tzz
val obj : ('a,'b) tzz -> ('a,'b) t

val empty: ('a,'b) t
          (** The empty map. *)
val add: 'a -> 'b -> ('a,'b) t -> ('a,'b) t
        (** [add x y m] returns a map containing the same bindings as
           [m], plus a binding of [x] to [y]. If [x] was already bound
           in [m], its previous binding disappears. *)
val find: 'a -> ('a,'b) t -> 'b
        (** [find x m] returns the current binding of [x] in [m],
           or raises [Not_found] if no such binding exists. *)
val remove: 'a -> ('a,'b) t -> ('a,'b) t
        (** [remove x m] returns a map containing the same bindings as
           [m], except for [x] which is unbound in the returned map. *)
val mem:  'a -> ('a,'b) t -> bool
        (** [mem x m] returns [true] if [m] contains a binding for [m],
           and [false] otherwise. *)
val addmap : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val interset : ('a,'b) t -> 'a Sette.t -> ('a,'b) t 
val diffset : ('a,'b) t -> 'a Sette.t -> ('a,'b) t 
val iter: ('a -> 'b -> unit) -> ('a,'b) t -> unit
        (** [iter f m] applies [f] to all bindings in map [m].
           [f] receives the key as first argument, and the associated value
           as second argument. The order in which the bindings are passed to
           [f] is unspecified. Only current bindings are presented to [f]:
           bindings hidden by more recent bindings are not passed to [f]. *)
val map: ('b -> 'c) -> ('a,'b) t -> ('a,'c) t
        (** [map f m] returns a map with same domain as [m], where the
           associated value [a] of all bindings of [m] has been
           replaced by the result of the application of [f] to [a].
           The order in which the associated values are passed to [f]
           is unspecified. *)
val mapi: ('a -> 'b -> 'c) -> ('a,'b) t -> ('a,'c) t
        (** Same as [map], but the function receives as arguments both the
           key and the associated value for each binding of the map. *)
val fold: ('a -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
        (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
           where [k1 ... kN] are the keys of all bindings in [m],
           and [d1 ... dN] are the associated data.
           The order in which the bindings are presented to [f] is
           unspecified. *)
val maptoset: ('a,'b) t -> 'a Sette.t
        (** [maptoset m] returns the set of the keys in the association table *)
val mapofset: ('a -> 'b) -> 'a Sette.t -> ('a,'b) t
        (** [mapofset f s] returns the map associating [f key] to
	   [key], for each element [key] of the set [s] *)
val compare: ?compare:('b -> 'b -> int) -> ('a,'b) t -> ('a,'b) t -> int
        (** Comparison function between maps *)
val filter: ('a -> 'b -> bool) -> ('a,'b) t -> ('a,'b) t
        (** [filter p m] returns the map of all bindings in [m] that satisfy
	  predicate [p]. *)
val partition: ('a -> 'b -> bool) -> ('a,'b) t -> ('a,'b) t * ('a,'b) t
        (** [partition p m] returns a pair of maps [(m1, m2)], where [m1] is
	  the map of all the bindings of [m] that satisfy the predicate [p],
	  and [m2] is the map of all the bindings of [m] that do not satisfy
	  [p]. *)
val cardinal: ('a,'b) t -> int
        (** Number of keys of a map *)
val print :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    ?firstbind:(unit, Format.formatter, unit) format ->
    ?sepbind:(unit, Format.formatter, unit) format ->
    ?lastbind:(unit, Format.formatter, unit) format ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) -> 
    Format.formatter -> ('a,'b) t -> unit
      (** *)

(** Output signature of the functor {!Mappe.Make}. *)
module type S = sig
  type key    
  type 'a t

  module Setkey : (Sette.S with type elt=key)

  val repr : 'a t -> (key,'a) tzz
  val obj : (key,'a) tzz -> 'a t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val addmap : 'a t -> 'a t -> 'a t
  val interset : 'a t -> Setkey.t -> 'a t 
  val diffset : 'a t -> Setkey.t -> 'a t 
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val maptoset : 'a t -> Setkey.t
  val mapofset: (key -> 'a) -> Setkey.t -> 'a t
  val compare : ?compare:('a -> 'a -> int) -> 'a t -> 'a t -> int
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val print :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    ?firstbind:(unit, Format.formatter, unit) format ->
    ?sepbind:(unit, Format.formatter, unit) format ->
    ?lastbind:(unit, Format.formatter, unit) format ->
    (Format.formatter -> key -> unit) ->
    (Format.formatter -> 'a -> unit) -> 
    Format.formatter -> 'a t -> unit
end

(** Functor building an implementation of the map structure
   given a totally ordered type. *)
module Make(Setkey : Sette.S) : (S with type key=Setkey.elt 
				   and module Setkey=Setkey)
