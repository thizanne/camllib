(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: weak.ml,v 1.14 2007/02/16 16:05:36 doligez Exp $ *)
(* Modified by Bertrand Jeannet, provides only weak hashtables *)
type ('a, 'b) hashtbl = {
  mutable table : ('a Weak.t * 'b) Weak.t array;
  mutable totsize : int;
  mutable limit : int;
}
type ('a, 'b) t = ('a, 'b) hashtbl
type 'a compare = { hash : 'a -> int; equal : 'a -> 'a -> bool; }
val create : int -> ('a, 'b) hashtbl
val clear : ('a, 'b) hashtbl -> unit
val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) hashtbl -> 'c -> 'c
val iter : ('a -> 'b -> 'c) -> ('a, 'b) hashtbl -> unit
val add : ('a, 'b) t -> 'a -> 'b -> unit
val find : ('a, 'b) hashtbl -> 'a -> 'b
val remove : ('a, 'b) hashtbl -> 'a -> unit
val mem : ('a, 'b) hashtbl -> 'a -> bool
val find_all : ('a, 'b) hashtbl -> 'a -> 'b list
val count : ('a, 'b) hashtbl -> int
val next_sz : int -> int
val stats : ('a, 'b) hashtbl -> int * int * int * int * int * int
val print :
  ?first:(unit, Format.formatter, unit) format ->
  ?sep:(unit, Format.formatter, unit) format ->
  ?last:(unit, Format.formatter, unit) format ->
  ?firstbind:(unit, Format.formatter, unit) format ->
  ?sepbind:(unit, Format.formatter, unit) format ->
  ?lastbind:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) hashtbl -> unit

module type S =
  sig
    type key
    type 'a t = (key, 'a) hashtbl
    val create : int -> 'a t
    val clear : 'a t -> unit
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val count : 'a t -> int
    val stats : 'a t -> int * int * int * int * int * int
    val print :
      ?first:(unit, Format.formatter, unit) format ->
      ?sep:(unit, Format.formatter, unit) format ->
      ?last:(unit, Format.formatter, unit) format ->
      ?firstbind:(unit, Format.formatter, unit) format ->
      ?sepbind:(unit, Format.formatter, unit) format ->
      ?lastbind:(unit, Format.formatter, unit) format ->
      (Format.formatter -> key -> unit) ->
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end

module Make (H : Hashtbl.HashedType) : (S with type key = H.t
					  and type 'a t = (H.t,'a) hashtbl)

module Custom :
  sig
    type ('a, 'b) t = { compare : 'a compare; hashtbl : ('a, 'b) hashtbl; }
    val create_compare : 'a compare -> int -> ('a, 'b) t
    val create : ('a -> int) -> ('a -> 'a -> bool) -> int -> ('a, 'b) t
    val clear : ('a, 'b) t -> unit
    val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
    val iter : ('a -> 'b -> 'c) -> ('a, 'b) t -> unit
    val count : ('a, 'b) t -> int
    val add : ('a, 'b) t -> 'a -> 'b -> unit
    val find : ('a, 'b) t -> 'a -> 'b
    val remove : ('a, 'b) t -> 'a -> unit
    val mem : ('a, 'b) t -> 'a -> bool
    val find_all : ('a, 'b) t -> 'a -> 'b list
    val stats : ('a, 'b) t -> int * int * int * int * int * int
    val print :
      ?first:(unit, Format.formatter, unit) format ->
      ?sep:(unit, Format.formatter, unit) format ->
      ?last:(unit, Format.formatter, unit) format ->
      ?firstbind:(unit, Format.formatter, unit) format ->
      ?sepbind:(unit, Format.formatter, unit) format ->
      ?lastbind:(unit, Format.formatter, unit) format ->
      (Format.formatter -> 'a -> unit) ->
      (Format.formatter -> 'b -> unit) ->
      Format.formatter -> ('a, 'b) t -> unit
  end
