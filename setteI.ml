(* $Id: setteI.ml,v 1.1 2003/11/12 21:56:38 bjeannet Exp $ *)

(** Sette specialized for keys of type int *)

module SetteI=Sette.Make(
  struct
    type t=int
    let compare=(-)
  end
)
include SetteI
