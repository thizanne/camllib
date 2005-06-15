
(* $Id: setteII.mli,v 1.1 2003/11/12 21:56:38 bjeannet Exp $ *)

(** Sette specialized for keys of type (int,int) *)

include (Sette.S with type elt=int*int)
