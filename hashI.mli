
(* $Id: hashI.mli,v 1.1 2003/11/12 21:56:37 bjeannet Exp $ *)

(** Hashtbl specialized for keys of type int *)

include Hashtbl.S with type key=int
