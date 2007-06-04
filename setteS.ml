(* $Id$ *)

(** Sette specialized for keys of type string *)

module SetteS=Sette.Make(
  struct
    type t=string
    let compare=String.compare
  end
)
include SetteS
