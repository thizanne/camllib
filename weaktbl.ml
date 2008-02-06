(***********************************************************************)
(*                                                                     *)
(*                            Weaktbl                                  *)
(*                                                                     *)
(*             (C) 2007 by Zheng Li (li@pps.jussieu.fr)                *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU Lesser General Public         *)
(*  License version 2.1 as published by the Free Software Foundation,  *)
(*  with the special exception on linking described in file LICENSE.   *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU Library General Public License for more details.               *)
(*                                                                     *)
(***********************************************************************)

(* Modified by Bertrand Jeannet (custom module) *)

(* weak stack, for ordering purpose *)
module Stack = struct
  type 'a t = {
    mutable data:'a Weak.t;
    mutable length:int;
    mutable cursor:int
  }
  let create (n:int) : 'a t =
    let len = min n (Sys.max_array_length - 1) in
    {data = Weak.create len; length = len; cursor = 0}
  let iter (f:'a -> unit) (s:'a t) :unit =
    for i = s.cursor -1 downto 0 do
      match Weak.get s.data i with Some x -> f x | _ -> ()
    done
  let length (s:'a t) : int =
    (* resize by the way, since it's invoked by push *)
    let flag = ref false and pt = ref 0 in
    for i = 0 to s.cursor -1 do
      match Weak.get s.data i with
      | Some x as d -> if !flag then Weak.set s.data !pt d; incr pt
      | None -> flag := true
    done;
    s.cursor <- !pt; s.cursor
  let copy (s:'a t) : 'a t =
    let s' = create s.length in
    Weak.blit s.data 0 s'.data 0 s.cursor; s'.cursor <- s.cursor; s'
  let rec push (x:'a) (s:'a t) =
    if s.cursor < s.length then
      (Weak.set s.data s.cursor (Some x); s.cursor <- s.cursor + 1)
    else
      let len = length s in
      if len >= s.length / 3 && len < s.length * 2 / 3 then push x s else
	let len' = min (len * 3 / 2 + 2) (Sys.max_array_length -1) in
	if len' = len then failwith "Weaktbl.Stack.push: stack cannnot grow"
	else
	  let data' = Weak.create len' in
	  Weak.blit s.data 0 data' 0 s.cursor;
	  s.data <- data'; s.length <- len'; push x s
  let rec pop (s:'a t) : 'a =
    if s.cursor <= 0 then raise Not_found;
    s.cursor <- s.cursor -1;
    match Weak.get s.data s.cursor with Some x -> x | None -> pop s
  let rec top (s:'a t) : 'a =
    if s.cursor <= 0 then raise Not_found;
    match Weak.get s.data (s.cursor -1) with
    | Some x -> x | None -> s.cursor <- s.cursor -1; top s
  let is_empty (s:'a t) : bool = (* stop as earlier as we can *)
    try iter (fun _ -> raise Not_found) s; true with Not_found -> false
end

type 'a compare = 'a Weakke.compare

type 'a box = 'a Weak.t
let enbox (k:'a) : 'a Weak.t =
  let w = Weak.create 1 in Weak.set w 0 (Some k); w
let unbox (bk:'a Weak.t) : 'a option =
  Weak.get bk 0

type 'a bind = 'a box * Obj.t
let bind_new (k:'a) (v:'b) : 'a bind =
  (enbox k, Obj.repr v)

type 'a cls = 'a bind Stack.t
let cls_new (bd:'a bind) : 'a bind Stack.t =
  let cls = Stack.create 1 in Stack.push bd cls; cls
let dummy (k:'a) : 'a bind Stack.t =
  cls_new (bind_new k ())
let rec top_bind (cls:'a cls) : 'a * 'b =
  let (bk,v) as bind = Stack.top cls in
  match unbox bk with
  | Some k -> k, (Obj.obj v)
  | _ -> assert (bind == Stack.pop cls); top_bind cls
let top_key (cls:'a cls) :'a =
  fst (top_bind cls)
let top_value (cls:'a cls) : 'b =
  snd (top_bind cls)
let all_bind (cls:'a cls) : ('a * 'b) list =
  let l = ref [] in
  let f (bk,v) = match unbox bk with
    | Some k -> l := (k, Obj.obj v) :: !l | _ -> () in
  Stack.iter f cls; List.rev !l
let all_key (cls:'a cls) : 'a list =
  List.map fst (all_bind cls)
let all_value (cls:'a cls) : 'b list =
  List.map snd (all_bind cls)

type ('a,'b) hashtbl = 'a cls Weakke.t
type ('a,'b) t = ('a,'b) hashtbl

let iter (f:'a -> 'b -> unit) (tbl:('a,'b) t) : unit =
  let f' (bk,v) =
    match unbox bk with
    | Some k -> f k (Obj.obj v)
    | None -> ()
  in
  Weakke.iter (Stack.iter f') tbl

let fold (f:'a -> 'b -> 'c -> 'c) (tbl:('a,'b) t) (accu:'c) :'c =
  let r = ref accu in
  let f' k v = r := f k v !r in
  iter f' tbl; !r

let length (tbl:('a,'b) t) =
  Weakke.fold
    (fun cls res -> res + (Stack.length cls))
    tbl
    0

let create = Weakke.create
let clear = Weakke.clear

let print
  ?(first : (unit, Format.formatter, unit) format = ("[@[<hv>" : (unit, Format.formatter, unit) format))
  ?(sep : (unit, Format.formatter, unit) format = (";@ ":(unit, Format.formatter, unit) format))
  ?(last : (unit, Format.formatter, unit) format = ("@]]":(unit, Format.formatter, unit) format))
  ?(firstbind : (unit, Format.formatter, unit) format = ("" : (unit, Format.formatter, unit) format))
  ?(sepbind : (unit, Format.formatter, unit) format = (" => ":(unit, Format.formatter, unit) format))
  ?(lastbind : (unit, Format.formatter, unit) format = ("":(unit, Format.formatter, unit) format))
  (print_key:Format.formatter -> 'a -> unit)
  (print_data:Format.formatter -> 'b -> unit)
  (formatter:Format.formatter)
  (hash:('a,'b) hashtbl)
  : unit
  =
  Format.fprintf formatter first;
  let firstitem = ref true in
  iter
    (begin fun key data ->
      if !firstitem then firstitem := false else Format.fprintf formatter sep;
      Format.fprintf formatter firstbind;
      print_key formatter key;
      Format.fprintf formatter sepbind;
      print_data formatter data;
      Format.fprintf formatter lastbind;
    end)
    hash;
  Format.fprintf formatter last

module Compare = struct
  let find_all compare (tbl:('a,'b) t) key =
    try all_value (Weakke.Compare.find compare tbl (dummy key))
    with Not_found-> []

  let find compare (tbl:('a,'b) t) key = top_value (Weakke.Compare.find compare tbl (dummy key))

  let add compare (tbl:('a,'b) t) key data =
    let bd = bind_new key data in
    let cls =
      try
	let c = Weakke.Compare.find compare tbl (dummy key) in
	Stack.push bd c; c
      with Not_found ->
	let c = cls_new bd in
	Weakke.Compare.add compare tbl c;
	c
    in
    let final _ = ignore bd; ignore cls in
    try Gc.finalise final key
    with Invalid_argument _ -> Gc.finalise final bd; Gc.finalise final cls

  let remove compare (tbl:('a,'b) t) key =
    try ignore (Stack.pop (Weakke.Compare.find compare tbl (dummy key)))
    with Not_found -> ()

  let replace compare (tbl:('a,'b) t) key data =
    remove compare tbl key; add compare tbl key data

  let mem compare (tbl:('a,'b) t) key =
    try ignore (find compare tbl key); true
    with Not_found -> false

  let copy compare (tbl:('a,'b) t) =
    let tbl'= Weakke.create (Weakke.count tbl * 3 / 2 + 2) in
    Weakke.iter (fun cls -> Weakke.Compare.add compare tbl' (Stack.copy cls)) tbl; tbl'
end

module type S = sig
  include Hashtbl.S
  val print :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    ?firstbind:(unit, Format.formatter, unit) format ->
    ?sepbind:(unit, Format.formatter, unit) format ->
    ?lastbind:(unit, Format.formatter, unit) format ->
    (Format.formatter -> key -> unit) ->
    (Format.formatter -> 'a -> unit) -> 
    Format.formatter -> 'a t -> unit
end
module Make (H: Hashtbl.HashedType) : (S with type key = H.t and type 'b t = (H.t,'b) hashtbl) = struct
  module HX = struct
    type t = H.t cls
    let hash x : int =
      try H.hash (top_key x) with Not_found -> 0
    let equal x y =
      try H.equal (top_key x) (top_key y) with Not_found -> false
  end
  module W = Weak.Make(HX)

  type key = H.t
  type 'b t = (H.t,'b) hashtbl

  let compare = { Weakke.hash = HX.hash; Weakke.equal = HX.equal }

  let create = create
  let clear = clear
  let find_all tbl key = Compare.find_all compare tbl key
  let find tbl key = Compare.find compare tbl key
  let add tbl key data = Compare.add compare tbl key data
  let remove tbl key = Compare.remove compare tbl key
  let replace tbl key data = Compare.replace compare tbl key data
  let mem tbl key = Compare.mem compare tbl key
  let copy tbl = Compare.copy compare tbl
  let iter = iter
  let fold = fold
  let length = length
  let print = print
end

let compare = {
  Weakke.hash = begin fun x ->
    try Hashtbl.hash (top_key x) with Not_found -> 0
  end;
  Weakke.equal = begin fun x y ->
    try (top_key x) = (top_key y) with Not_found -> false
  end
}
let find_all tbl key = Compare.find_all compare tbl key
let find tbl key = Compare.find compare tbl key
let add tbl key data = Compare.add compare tbl key data
let remove tbl key = Compare.remove compare tbl key
let replace tbl key data = Compare.replace compare tbl key data
let mem tbl key = Compare.mem compare tbl key
let copy tbl = Compare.copy compare tbl

module Custom = struct
  type ('a,'b) t = 'a cls Weakke.Custom.t

  let create (hash:'a -> int) (equal:'a -> 'a -> bool) (n:int) : ('a,'b) t =
    let hash x =
      try hash (top_key x) with Not_found -> 0
    in
    let equal x y =
      try equal (top_key x) (top_key y) with Not_found -> false
    in
    Weakke.Custom.create hash equal n

  let clear = Weakke.Custom.clear
  let find_all tbl key =
    Compare.find_all tbl.Weakke.Custom.compare tbl.Weakke.Custom.hashtbl key
  let find tbl key =
    Compare.find tbl.Weakke.Custom.compare tbl.Weakke.Custom.hashtbl key
  let add tbl key data =
    Compare.add tbl.Weakke.Custom.compare tbl.Weakke.Custom.hashtbl key data
  let remove tbl key =
    Compare.remove tbl.Weakke.Custom.compare tbl.Weakke.Custom.hashtbl key
  let replace tbl key data =
    Compare.replace tbl.Weakke.Custom.compare tbl.Weakke.Custom.hashtbl key data
  let mem tbl key =
    Compare.mem tbl.Weakke.Custom.compare tbl.Weakke.Custom.hashtbl key
  let copy tbl = { tbl with
    Weakke.Custom.hashtbl = Compare.copy tbl.Weakke.Custom.compare tbl.Weakke.Custom.hashtbl
  }
  let iter f t = iter f t.Weakke.Custom.hashtbl
  let fold f t acc = fold f t.Weakke.Custom.hashtbl acc
  let length t = length t.Weakke.Custom.hashtbl
  let print ?first ?sep ?last ?firstbind ?sepbind ?lastbind 
    print_key print_data
    formatter t 
    =
    print ?first ?sep ?last ?firstbind ?sepbind ?lastbind 
    print_key print_data
    formatter t.Weakke.Custom.hashtbl

end
