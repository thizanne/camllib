
(* $Id: hashI.ml,v 1.2 2005/06/14 14:21:05 bjeannet Exp $ *)

(** Hashtbl specialized for keys of type int *)

module HashI=Hashtbl.Make(
  struct 
    type t=int
    external hash : int -> int = "%identity"
    let equal = (==)
  end
)
include HashI
