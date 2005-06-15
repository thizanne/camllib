
(* $Id: hashII.ml,v 1.1 2005/06/14 14:27:39 bjeannet Exp $ *)

(** Hashtbl specialized for keys of type int*int *)

module M=Hashtbl.Make
  (
    struct 
      type t=int*int
      let hash (x,y) = 16*x + y
      let equal (x1,x2) (y1,y2) = x1==y1 && x2==y2
    end
  )

include M
