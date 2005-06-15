
(* $Id: hashI.ml,v 1.2 2005/06/14 14:21:05 bjeannet Exp $ *)

(** Hashtbl specialized for keys of type int *)

module HashI=Hashtbl.Make(
  struct 
    type t=int
    external hash : int -> int = "%identity"
    let equal = (==)
  end
)
include HashI
let print
  ?(first : (unit, Format.formatter, unit) format option)
  ?(sep : (unit, Format.formatter, unit) format option)
  ?(last : (unit, Format.formatter, unit) format option)
  ?(firstbind : (unit, Format.formatter, unit) format option)
  ?(sepbind : (unit, Format.formatter, unit) format option)
  ?(lastbind : (unit, Format.formatter, unit) format option)
  (print_key:Format.formatter -> key -> unit)
  (print_data:Format.formatter -> 'a -> unit) 
  (formatter:Format.formatter)
  (hash:'a t)
  : unit
  =
  let (hash:(key,'a) Hashtbl.t) = Obj.magic hash in
  Print.hash ?first ?sep ?last ?firstbind ?sepbind ?lastbind
    print_key print_data
    formatter hash
