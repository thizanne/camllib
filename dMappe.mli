(* $Id: dMappe.mli,v 1.4 2003/11/12 20:39:02 bjeannet Exp $ *)

(** Two-way map between two ordered data types *)

(** Functional semantics of operations.  The generic interface assumess that
  types ['a] and ['b] should be comparable with the standard
  [Pervasives.compare] function. *) 

type ('a, 'b) t
  (** The type of two-way maps *)
val empty : ('a, 'b) t
  (** Empty map *)
val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  (** Add a new binding to the current map and return the new map. *)
val y_of_x : 'a -> ('a, 'b) t -> 'b
  (** Association *)
val x_of_y : 'b -> ('a, 'b) t -> 'a
  (** Inverse association *)
val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
  (** Remove a binding defined by its first element and return the new map. *)
val memx : 'a -> ('a, 'b) t -> bool
  (** Is the object in the map ? *)
val memy : 'b -> ('a, 'b) t -> bool
  (** Is the object in the map ? *)
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  (** Iterate on bindings. *)
val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  (** Iterate on bindings and accumulating a result. *)
val setx : ('a, 'b) t -> 'a Sette.t
  (** Return the set of all objects in the first place of bindings. *)
val sety : ('a, 'b) t -> 'b Sette.t
  (** Return the set of all objects in the second place of bindings. *)
val cardinal : ('a, 'b) t -> int
  (** Return the number of bindings. *)
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
    (** Print the set of bindings. *)

(** Input signature of the functor {!DMappe.Make}. *)
module type Param = sig
  module MappeX : Mappe.S
  module MappeY : Mappe.S
end

(** Output signature of the functor {!DMappe.Make}. *)
module type S = sig
  module MappeX : Mappe.S
  module MappeY : Mappe.S
  type x = MappeX.key
  type y = MappeY.key
  type t
  val empty : t
  val add : x -> y -> t -> t
  val y_of_x : x -> t -> y
  val x_of_y : y -> t -> x
  val remove : x -> t -> t 
  val memx : x -> t -> bool
  val memy : y -> t -> bool
  val iter : (x -> y -> unit) -> t -> unit
  val fold : (x -> y -> 'c -> 'c) -> t -> 'c -> 'c
  val setx : t -> MappeX.Setkey.t
  val sety : t -> MappeY.Setkey.t
  val cardinal : t -> int
  val print :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    ?firstbind:(unit, Format.formatter, unit) format ->
    ?sepbind:(unit, Format.formatter, unit) format ->
    ?lastbind:(unit, Format.formatter, unit) format ->
    (Format.formatter -> x -> unit) ->
    (Format.formatter -> y -> unit) -> 
    Format.formatter -> t -> unit
end

(** Functor building an implementation of the DMappe structure
   given two map structures. *)
module Make(P : Param) :
  S with module MappeX = P.MappeX 
    and module MappeY = P.MappeY
