(* $Id: mappeI.mli,v 1.1 2003/11/12 21:56:37 bjeannet Exp $ *)

(** Mappe specialized for keys of type int *)

include (Mappe.S with type key=int and module Setkey=SetteI)
