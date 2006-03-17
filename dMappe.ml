(* $Id: dMappe.ml,v 1.5 2003/11/12 20:39:01 bjeannet Exp $ *)

(** Two-way map between two ordered data types *)

(** The type of two-way maps *)
type ('a,'b) t = {
  xy : ('a,'b) Mappe.t;
  yx : ('b,'a) Mappe.t
}

let empty = { xy = Mappe.empty; yx = Mappe.empty }
let add x y t = {
  xy = Mappe.add x y t.xy;
  yx = Mappe.add y x t.yx
}
let y_of_x x t = Mappe.find x t.xy
let x_of_y y t =  Mappe.find y t.yx
let remove x t = {
  xy = Mappe.remove x t.xy;
  yx = Mappe.remove (y_of_x x t) t.yx
}
let memx x t = Mappe.mem x t.xy
let memy y t = Mappe.mem y t.yx
let iter f t = Mappe.iter f t.xy
let fold f t v = Mappe.fold f t.xy v
let setx t = Mappe.maptoset t.xy
let sety t = Mappe.maptoset t.yx
let cardinal t = Mappe.cardinal t.xy
let print ?first ?sep ?last ?firstbind ?(sepbind=(" <=> ":(unit, Format.formatter, unit) format)) ?lastbind px py fmt t =
  Mappe.print 
    ?first ?sep ?last 
    ?firstbind ~sepbind ?lastbind
    px py fmt t.xy

(** Input signature of the functor {!DMappe.Make}. *)
module type Param = sig
  module MappeX : Mappe.S
  module MappeY : Mappe.S
end

(** Output signature of the functor {!DMappe.Make}. *)
module type S = sig
  module MappeX : Mappe.S
  module MappeY : Mappe.S
  type x = MappeX.key
  type y = MappeY.key
  type t
  val empty : t
  val add : x -> y -> t -> t
  val y_of_x : x -> t -> y
  val x_of_y : y -> t -> x
  val remove : x -> t -> t 
  val memx : x -> t -> bool
  val memy : y -> t -> bool
  val iter : (x -> y -> unit) -> t -> unit
  val fold : (x -> y -> 'c -> 'c) -> t -> 'c -> 'c
  val setx : t -> MappeX.Setkey.t
  val sety : t -> MappeY.Setkey.t
  val cardinal : t -> int
  val print :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    ?firstbind:(unit, Format.formatter, unit) format ->
    ?sepbind:(unit, Format.formatter, unit) format ->
    ?lastbind:(unit, Format.formatter, unit) format ->
    (Format.formatter -> x -> unit) ->
    (Format.formatter -> y -> unit) -> 
    Format.formatter -> t -> unit
end

(** Functor building an implementation of the DMappe structure
   given two map structures. *)
module Make(P : Param) = struct
  module MappeX = P.MappeX
  module MappeY = P.MappeY
  type x = MappeX.key
  type y = MappeY.key
  type t = {
    xy : y MappeX.t;
    yx : x MappeY.t
  }
  let empty = { xy = MappeX.empty; yx = MappeY.empty }
  let add x y t = {
    xy = MappeX.add x y t.xy;
    yx = MappeY.add y x t.yx;
  }
  let y_of_x x t = MappeX.find x t.xy
  let x_of_y y t = MappeY.find y t.yx
  let remove x t = {
    xy = MappeX.remove x t.xy;
    yx = MappeY.remove (y_of_x x t) t.yx
  }
  let memx x t = MappeX.mem x t.xy
  let memy y t = MappeY.mem y t.yx
  let iter f t = MappeX.iter f t.xy
  let fold f t v = MappeX.fold f t.xy v
  let setx t = MappeX.maptoset t.xy
  let sety t = MappeY.maptoset t.yx
  let cardinal t = MappeX.cardinal t.xy
  let print ?first ?sep ?last ?firstbind ?(sepbind = (" <=> ":(unit, Format.formatter, unit) format)) ?lastbind px py fmt t = 
    MappeX.print ?first ?sep ?last ?firstbind ~sepbind ?lastbind px py fmt t.xy
end
