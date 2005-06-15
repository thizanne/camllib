
(* $Id: hashIB.ml,v 1.3 2005/06/14 14:21:05 bjeannet Exp $ *)

(** Hashtbl specialized for keys of type (int,bool) *)

module HashIB=Hashtbl.Make
  (
    struct 
      type t=int*bool
      let hash (id,b) = 
	let h = 2*id in
	abs(if b then h+1 else h)
      let equal (id1,b1) (id2,b2) = id1==id2 && b1==b2
    end
  )

include HashIB


