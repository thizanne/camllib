
(* $Id: hashII.mli,v 1.1 2005/06/14 14:27:40 bjeannet Exp $ *)

(** Hashtbl specialized for keys of type int*int *)

include Hashtbl.S with type key=int*int
