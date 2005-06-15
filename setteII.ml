
(* $Id: setteII.ml,v 1.1 2003/11/12 21:56:38 bjeannet Exp $ *)

(** Sette specialized for keys of type (int,int) *)

module SetteII=Sette.Make(
  struct
    type t=int*int
    let compare (x1,y1) (x2,y2) = 
      let res = x1-x2 in
      if res==0 then
	y1-y2
      else
	res
  end
)
include SetteII
