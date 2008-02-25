(***********************************************************************)
(*                                                                     *)
(*                            Weaktbl                                  *)
(*                                                                     *)
(*             (C) 2007 by Zheng Li (li@pps.jussieu.fr)                *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU Lesser General Public         *)
(*  License version 2.1 as published by the Free Software Foundation,  *)
(*  with the special exception on linking described in file LICENSE.   *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU Library General Public License for more details.               *)
(*                                                                     *)
(***********************************************************************)

(** Weak hash table library for OCaml, with an interface compatible with
    the standard Hashtbl module.
*)

(*
module Stack : sig
  type 'a t
end
type 'a box = 'a Weak.t
type 'a bind = 'a box * Obj.t
type 'a cls = 'a bind Stack.t
type ('a,'b) hashtbl = 'a cls Weakke.t
*)

type ('a,'b) hashtbl

(** {2 Generic interface} *)


type ('a, 'b) t = ('a,'b) hashtbl
(** The type of hash tables from type ['a] to type ['b]. *)

val create : int -> ('a, 'b) t
(** [Weaktbl.create n] creates a new, empty hash table, with
   initial size [n].  For best results, [n] should be on the
   order of the expected number of elements that will be in
   the table.  The table grows as needed, so [n] is just an
   initial guess. *)

val clear : ('a, 'b) t -> unit
(** Empty a hash table. *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
(** [Weaktbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
   Previous bindings for [x] are not removed, but simply
   hidden. That is, after performing {!Weaktbl.remove}[ tbl x],
   the previous binding for [x], if any, is restored.
   (Same behavior as with association lists.) *)

val copy : ('a, 'b) t -> ('a, 'b) t
(** Return a copy of the given hashtable. *)

val find : ('a, 'b) t -> 'a -> 'b
(** [Weaktbl.find tbl x] returns the current binding of [x] in [tbl],
   or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b) t -> 'a -> 'b list
(** [Weaktbl.find_all tbl x] returns the list of all data
   associated with [x] in [tbl].
   The current binding is returned first, then the previous
   bindings, in reverse order of introduction in the table. *)

val mem : ('a, 'b) t -> 'a -> bool
(** [Weaktbl.mem tbl x] checks if [x] is bound in [tbl]. *)

val remove : ('a, 'b) t -> 'a -> unit
(** [Weaktbl.remove tbl x] removes the current binding of [x] in [tbl],
   restoring the previous binding if it exists.
   It does nothing if [x] is not bound in [tbl]. *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(** [Weaktbl.replace tbl x y] replaces the current binding of [x]
   in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
   a binding of [x] to [y] is added to [tbl].
   This is functionally equivalent to {!Weaktbl.remove}[ tbl x]
   followed by {!Weaktbl.add}[ tbl x y]. *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(** [Weaktbl.iter f tbl] applies [f] to all bindings in table [tbl].
   [f] receives the key as first argument, and the associated value
   as second argument. Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first. *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** [Weaktbl.fold f tbl init] computes
   [(f kN dN ... (f k1 d1 init)...)],
   where [k1 ... kN] are the keys of all bindings in [tbl],
   and [d1 ... dN] are the associated values.
   Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first. *)


val length : ('a, 'b) t -> int
(** [Weaktbl.length tbl] returns the number of bindings in [tbl].
   Multiple bindings are counted multiply, so [Weaktbl.length]
   gives the number of times [Weaktbl.iter] calls its first argument. *)

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

(** {2 Functorial interface} *)

module type S = sig
  include Hashtbl.S
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
module Make (H : Hashtbl.HashedType) : S with type key = H.t and type 'a t = (H.t,'a) hashtbl
(** Functor building an implementation of the hashtable structure.
    The functor [Weaktbl.Make] returns a structure containing
    a type [key] of keys and a type ['a t] of hash tables
    associating data of type ['a] to keys of type [key].
    The operations perform similarly to those of the generic
    interface, but use the hashing and equality functions
    specified in the functor argument [H] instead of generic
    equality and hashing. *)

(** {2 Custom interface} *)
						 
module Custom : sig
  type ('a,'b) t
  val create : ('a -> int) -> ('a -> 'a -> bool) -> int -> ('a, 'b) t
  val clear : ('a,'b) t -> unit
  val find_all : ('a,'b) t -> 'a -> 'b list
  val find : ('a,'b) t -> 'a -> 'b
  val add : ('a,'b) t -> 'a -> 'b -> unit
  val remove : ('a,'b) t -> 'a -> unit
  val replace : ('a,'b) t -> 'a -> 'b -> unit
  val mem : ('a,'b) t -> 'a -> bool
  val copy : ('a,'b) t -> ('a,'b) t
  val iter : ('a -> 'b -> unit) -> ('a,'b) t -> unit
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
  val length : ('a,'b) t -> int
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
end
