(* $Id: dHashtbl.ml,v 1.3 2005/06/14 14:13:40 bjeannet Exp $ *)

(** Two-way hashtable between two data types *)

open Format

(** The type of two-way hashtables *)
type ('a,'b) t = {
  xy : ('a,'b) Hashtbl.t;
  yx : ('b,'a) Hashtbl.t
}

let clear t = 
  Hashtbl.clear t.xy; 
  Hashtbl.clear t.yx;
  ()

let create size = { xy = Hashtbl.create size; yx = Hashtbl.create size }
let add t x y =
  Hashtbl.add t.xy x y;
  Hashtbl.add t.yx y x

let y_of_x t x = Hashtbl.find t.xy x
let x_of_y t y = Hashtbl.find t.yx y
let removex t x = 
  let y = y_of_x t x in
  Hashtbl.remove t.xy x;
  Hashtbl.remove t.yx y
let removey t y = 
  let x = x_of_y t y in
  Hashtbl.remove t.xy x;
  Hashtbl.remove t.yx y

let memx t x = Hashtbl.mem t.xy x
let memy t y = Hashtbl.mem t.yx y
let iter t f = Hashtbl.iter f t.xy
let fold t v f = Hashtbl.fold f t.xy v
let cardinal t = Hashtbl.fold (fun x y res -> res+1) t.xy 0
let print 
  ?(first : (unit, Format.formatter, unit) format = ("[@[<hv>" : (unit, Format.formatter, unit) format))
  ?(sep : (unit, Format.formatter, unit) format = (";@ ":(unit, Format.formatter, unit) format))
  ?(last : (unit, Format.formatter, unit) format = ("@]]":(unit, Format.formatter, unit) format))
  ?(firstbind : (unit, Format.formatter, unit) format = ("" : (unit, Format.formatter, unit) format))
  ?(sepbind : (unit, Format.formatter, unit) format = (" => ":(unit, Format.formatter, unit) format))
  ?(lastbind : (unit, Format.formatter, unit) format = ("":(unit, Format.formatter, unit) format))
  px py fmt t 
  = 
  Print.hash ~first ~sep ~last ~firstbind ~sepbind ~lastbind
    px py
    fmt t.xy

(** Input signature of the functor {!DHashtbl.Make}. *)
module type Param = sig
  include DHashtbl.Param
end

(** Output signature of the functor {!DHashtbl.Make}. *)
module type S = sig
  include DHashtbl.S
end

(** Functor building an implementation of the DHashtbl structure
   given two hashtables *)
module Make(P : Param) = struct
  module HashX = P.HashX
  module HashY = P.HashY
  type x = HashX.key
  type y = HashY.key
  type t = {
    xy : y HashX.t;
    yx : x HashY.t
  }
  let clear t = 
    HashX.clear t.xy; 
    HashY.clear t.yx;
    ()

  let create size = { xy = HashX.create size; yx = HashY.create size }
  let add t x y =
    HashX.add t.xy x y;
    HashY.add t.yx y x
  let y_of_x t x = HashX.find t.xy x
  let x_of_y t y = HashY.find t.yx y
  let removex t x = 
    let y = y_of_x t x in
    HashX.remove t.xy x;
    HashY.remove t.yx y
  let removey t y = 
    let x = x_of_y t y in
    HashX.remove t.xy x;
    HashY.remove t.yx y

  let memx t x = HashX.mem t.xy x
  let memy t y = HashY.mem t.yx y
  let iter t f = HashX.iter f t.xy
  let fold t v f = HashX.fold f t.xy v
  let cardinal t = HashX.fold (fun x y res -> res+1) t.xy 0
  let print 
    ?(first : (unit, Format.formatter, unit) format option)
    ?(sep : (unit, Format.formatter, unit) format option)
    ?(last : (unit, Format.formatter, unit) format option)
    ?(firstbind : (unit, Format.formatter, unit) format option)
    ?(sepbind : (unit, Format.formatter, unit) format = (" <=> ":(unit, Format.formatter, unit) format))
    ?(lastbind : (unit, Format.formatter, unit) format option)
    px py fmt t 
    = 
    let (hash:(x,y) Hashtbl.t) = Obj.magic t.xy in
    Print.hash ?first ?sep ?last ?firstbind ~sepbind ?lastbind
    px py
    fmt hash

end

