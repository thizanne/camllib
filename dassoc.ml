(* $Id: dassoc.ml,v 1.3 2003/11/12 20:39:02 bjeannet Exp $ *)

(** Two-way association table between identifiers and objects *)

module type S = sig
  include Dassoc.S
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

