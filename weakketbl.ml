(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: weak.ml,v 1.14 2007/02/16 16:05:36 doligez Exp $ *)
(* Modified by Bertrand Jeannet, provides only weak hashtables *)

type ('a,'b) hashtbl = {
  mutable table : ('a Weak.t * 'b) Weak.t array;
  mutable totsize : int;             (* sum of the bucket sizes *)
  mutable limit : int;               (* max ratio totsize/table length *)
}
type ('a,'b) t = ('a,'b) hashtbl

type 'a compare = {
  hash : 'a -> int;
  equal : 'a -> 'a -> bool;
}

let box k =
  let wk = Weak.create 1 in
  Weak.set wk 0 (Some k);
  wk
let unbox wk =
  Weak.get wk 0

let weak_get bucket i = 
  match Weak.get bucket i with
  | Some (wk,d) -> 
      begin match unbox wk with
      | Some k -> Some (k,d)
      | None ->
	  Weak.set bucket i None;
	  None
      end
  | None -> None

let weak_get_copy = Weak.get

let create sz =
  let emptybucket = Weak.create 0 in
  let sz = if sz < 7 then 7 else sz in
  let sz = if sz > Sys.max_array_length then Sys.max_array_length else sz in
  {
    table = Array.create sz emptybucket;
    totsize = 0;
    limit = 3;
  }

let clear t =
  let emptybucket = Weak.create 0 in
  for i = 0 to Array.length t.table - 1 do
    t.table.(i) <- emptybucket;
  done;
  t.totsize <- 0;
  t.limit <- 3

let fold f t init =
  let rec fold_bucket i b accu =
    if i >= Weak.length b then accu else
      match weak_get b i with
      | Some (k,d) -> fold_bucket (i+1) b (f k d accu)
      | None -> fold_bucket (i+1) b accu
  in
  Array.fold_right (fold_bucket 0) t.table init

let iter f t =
  let rec iter_bucket i b =
    if i >= Weak.length b then () else
      match weak_get b i with
      | Some (k,d) -> f k d; iter_bucket (i+1) b
      | None -> iter_bucket (i+1) b
  in
  Array.iter (iter_bucket 0) t.table

let count t =
  let rec count_bucket i b accu =
    if i >= Weak.length b then accu else
      let n = 
	match weak_get b i with
	| Some (k,d) -> 1
	| None -> 0
      in
      count_bucket (i+1) b (accu + n)
  in
  Array.fold_right (count_bucket 0) t.table 0

let next_sz n = min (3*n/2 + 3) (Sys.max_array_length - 1)

let stats t =
  let len = Array.length t.table in
  let lens = Array.map Weak.length t.table in
  Array.sort compare lens;
  let totlen = Array.fold_left ( + ) 0 lens in
  (len, count t, totlen, lens.(0), lens.(len/2), lens.(len-1))

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

  let get_index compare t k =
    (compare.hash k land max_int) mod (Array.length t.table);;

  let rec resize compare t =
    let oldlen = Array.length t.table in
    let newlen = next_sz oldlen in
    if newlen > oldlen then begin
      let newt = create newlen in
      newt.limit <- t.limit + 100;          (* prevent resizing of newt *)
      fold (fun k d () -> add compare newt k d) t ();
   (* assert Array.length newt.table = newlen; *)
      t.table <- newt.table;
   (* t.limit <- t.limit + 2; -- performance bug *)
    end

  and add_aux compare t k d index =
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then begin
	let newsz = min (sz + 3) (Sys.max_array_length - 1) in
	if newsz <= sz then failwith "Weak.Make : hash bucket cannot grow more";
	let newbucket = Weak.create newsz in
	Weak.blit bucket 0 newbucket 0 sz;
	Weak.set newbucket i (Some ((box k), d));
	t.table.(index) <- newbucket;
	t.totsize <- t.totsize + (newsz - sz);
	if t.totsize > t.limit * Array.length t.table then resize compare t;
      end else begin
	if Weak.check bucket i
	then loop (i+1)
	else Weak.set bucket i (Some ((box k), d));
      end
    in
    loop 0;

  and add compare (t:('a,'b) t) (k:'a) (d:'b) = 
    add_aux compare t k d (get_index compare t k)
    
  let find_or (compare:'a compare) t key ifnotfound =
    let index = get_index compare t key in
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then ifnotfound index
      else begin
	match weak_get bucket i with
	| Some (k,d) when compare.equal k key -> d
	| _ -> loop (i+1)
      end
    in
    loop 0

  let find (compare:'a compare) t k =
    find_or compare t k (fun index -> raise Not_found)

  let find_shadow (compare:'a compare) t key iffound ifnotfound =
    let index = get_index compare t key in
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then ifnotfound else begin
	match weak_get bucket i with
	| Some (k,d) when compare.equal k key -> iffound bucket i
	| _ -> loop (i+1)
      end
    in
    loop 0
      
  let remove (compare:'a compare) t k =
    find_shadow compare t k (fun w i -> Weak.set w i None) ()
      
  let mem (compare:'a compare) t k =
    find_shadow compare t k (fun w i -> true) false
      
  let find_all compare t key =
    let index = get_index compare t key in
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i accu =
      if i >= sz then accu
      else begin
	match weak_get bucket i with
	| Some (k,d) when compare.equal k key -> loop (i+1) (d::accu)
	| _ -> loop (i+1) accu
      end
    in
    loop 0 []
      
end

(** Weak hash tables *)

module type S = sig
  type key
  type 'a t = (key,'a) hashtbl
  val create : int -> 'a t
  val clear : 'a t -> unit
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_all : 'a t -> key -> 'a list
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val count : 'a t -> int
  val stats : 'a t -> int * int * int * int * int * int
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

module Make (H : Hashtbl.HashedType) : (S with type key = H.t
					  and type 'a t = (H.t,'a) hashtbl) =
struct
  type key = H.t
  type 'a t = (key,'a) hashtbl

  let compare = { hash = H.hash; equal = H.equal }

  let create = create
  let clear = clear
  let fold = fold
  let iter = iter
  let count = count
  let add t k d = Compare.add compare t k d
  let find t k = Compare.find compare t k
  let remove t k = Compare.remove compare t k
  let mem t k = Compare.mem compare t k
  let find_all t k = Compare.find_all compare t k
  let stats = stats
  let print = print
end

module Custom = struct
  type ('a,'b) t = {
    compare : 'a compare;
    hashtbl:('a,'b) hashtbl;
  }

  let create_compare (cmp:'a compare) n = 
    {
      compare = cmp;
      hashtbl = create n
    }
  let create (hash:'a -> int) (equal:'a -> 'a -> bool) n = 
    let compare = { 
	hash=(fun x -> (hash x) land max_int);
	equal=equal 
      }
    in
    create_compare compare n

  let clear t = clear t.hashtbl
  let fold f t init = fold f t.hashtbl init
  let iter f t = iter f t.hashtbl
  let count t = count t.hashtbl
  let add t data = Compare.add t.compare t.hashtbl data
  let find t data = Compare.find t.compare t.hashtbl data
  let remove t data = Compare.remove t.compare t.hashtbl data
  let mem t data = Compare.mem t.compare t.hashtbl data
  let find_all t data = Compare.find_all t.compare t.hashtbl data
  let stats t = stats t.hashtbl
  let print ?first ?sep ?last ?firstbind ?sepbind ?lastbind pa pb fmt t = 
    print ?first ?sep ?last ?firstbind ?sepbind ?lastbind pa pb fmt t.hashtbl
end


let compare = {
  hash=Hashtbl.hash;
  equal=(=)
}

let add t k d = Compare.add compare t k d
let find t key = Compare.find compare t key
let remove t key = Compare.remove compare t key
let mem t key = Compare.mem compare t key
let find_all t key = Compare.find_all compare t key
