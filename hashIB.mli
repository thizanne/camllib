
(* $Id: hashIB.mli,v 1.1 2003/11/12 21:56:37 bjeannet Exp $ *)

(** Hashtbl specialized for keys of type (int,bool) *)

include Hashtbl.S with type key=int*bool
