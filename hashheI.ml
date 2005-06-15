
(* $Id: hashheI.ml,v 1.1 2005/06/14 14:27:40 bjeannet Exp $ *)

(** Hashtbl specialized for keys of type int *)

module M=Hashhe.Make(
  struct 
    type t=int
    external hash : int -> int = "%identity"
    let equal = (==)
  end
)
include M
