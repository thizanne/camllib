
(* $Id: hashheIB.mli,v 1.1 2005/06/14 14:27:40 bjeannet Exp $ *)

(** Hashtbl specialized for keys of type (int,bool) *)

include Hashhe.S with type key=int*bool
