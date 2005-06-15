(* $Id: graphI.mli,v 1.1 2003/11/13 16:01:33 bjeannet Exp $ *)

(** Graphs specialized for vertices of type int *)

include (Graph.S with type vertex=int
		 and module SetV=SetteI
		 and module MapV=MappeI
		 and module MapE=MappeII)
