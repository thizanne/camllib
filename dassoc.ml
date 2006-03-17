(* $Id: dassoc.ml,v 1.3 2003/11/12 20:39:02 bjeannet Exp $ *)

(** Two-way association table between identifiers and objects *)

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

module Make(Hash : Hashtbl.S) = struct
  type obj = Hash.key
  type 'a t = {
    hash : 'a Hash.t;
    mutable map : ('a, obj) Mappe.t
  }

  let create () = { 
    hash = Hash.create 23;
    map = Mappe.empty
  }

  let id_of_obj tbl obj = Hash.find tbl.hash obj
  let obj_of_id tbl id = Mappe.find id tbl.map
  let mem tbl id = Mappe.mem id tbl.map

  let add tbl id obj =
    Hash.add tbl.hash obj id;
    tbl.map <- Mappe.add id obj tbl.map

  let remove tbl id =
    let obj = obj_of_id tbl id in
    Hash.remove tbl.hash obj;
    tbl.map <- Mappe.remove id tbl.map

  let iter f tbl = Mappe.iter f tbl.map
  let fold f tbl v = Mappe.fold f tbl.map v

  let set tbl = Mappe.maptoset tbl.map
  let map tbl = tbl.map

  let clear tbl =
    Hash.clear tbl.hash;
    tbl.map <- Mappe.empty

end

