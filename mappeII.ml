
(* $Id: mappeII.ml,v 1.1 2003/11/12 21:56:37 bjeannet Exp $ *)

(** Mappe specialized for keys of type (int,int) *)

module MappeII = Mappe.Make(SetteII)

include MappeII
