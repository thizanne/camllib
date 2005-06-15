(* $Id: dassoc.mli,v 1.2 2003/11/12 20:39:02 bjeannet Exp $ *)

(** Two-way association table between identifiers and objects *)

(** Association from identifiers to objects is performed with an generic
  ordered balanced association tree. Association from objects to identifiers
  is performed with a specialized hash table. This means that identifiers should be
  comparable with the standard [Pervasives.compare] function. The semantics of
  operations is a side-effect one, not a functional one.
*)

(** Output signature of the functor {!Dassoc.Make}. *)
module type S = sig
  type obj
    (** The type of objects *)
  type 'a t
    (** The type of two-way association table, with ['a] the type of identifiers. *)
  val create : unit -> 'a t
    (** Create a two-way association table.*)
  val id_of_obj : 'a t -> obj -> 'a
    (** Return the identifier corresponding to the object *)
  val obj_of_id : 'a t -> 'a -> obj
    (** Return the object corresponding to the identifier *)
  val mem : 'a t -> 'a -> bool
    (** Is the identifier registered in the table ? *)
  val add : 'a t -> 'a -> obj -> unit
    (** Add a new binding [(id,obj)]. *)
  val remove : 'a t -> 'a -> unit
    (** Remove the binding between the given identifier and its associated object. *)
  val iter : ('a -> obj -> unit) -> 'a t -> unit
    (** Iterate on bindings. *)
  val fold : ('a -> obj -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** Iterate on bindings and accumulating a result. *)
  val set : 'a t -> 'a Sette.t
    (** Return the set of identifiers *)
  val map : 'a t -> ('a, obj) Mappe.t
    (** Return a (single) association table from identifiers to objects *)
  val clear : 'a t -> unit
    (** Clear all bindings in the table. *)
end

module Make(Hash : Hashtbl.S) :
  (S with type obj = Hash.key)
(** Functor building an implementation of the double association table
  structure, given an hashtable module for objects *)

