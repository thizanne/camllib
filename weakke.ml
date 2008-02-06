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

type 'a hashtbl = {
  mutable table : 'a Weak.t array;
  mutable totsize : int;             (* sum of the bucket sizes *)
  mutable limit : int;               (* max ratio totsize/table length *)
}
type 'a t = 'a hashtbl

type 'a compare = {
  hash : 'a -> int;
  equal : 'a -> 'a -> bool;
}

let weak_create = Weak.create

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
      match Weak.get b i with
      | Some v -> fold_bucket (i+1) b (f v accu)
      | None -> fold_bucket (i+1) b accu
  in
  Array.fold_right (fold_bucket 0) t.table init

let iter f t =
  let rec iter_bucket i b =
    if i >= Weak.length b then () else
      match Weak.get b i with
      | Some v -> f v; iter_bucket (i+1) b
      | None -> iter_bucket (i+1) b
  in
  Array.iter (iter_bucket 0) t.table

let count t =
  let rec count_bucket i b accu =
    if i >= Weak.length b then accu else
      count_bucket (i+1) b (accu + (if Weak.check b i then 1 else 0))
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
  (print_data:Format.formatter -> 'a -> unit)
  (formatter:Format.formatter)
  (hash:'a hashtbl)
  : unit
  =
  Format.fprintf formatter first;
  let firstitem = ref true in
  iter
    (begin fun data ->
      if !firstitem then firstitem := false else Format.fprintf formatter sep;
      print_data formatter data;
    end)
    hash;
  Format.fprintf formatter last

module Compare = struct

  let get_index compare t d =
    (compare.hash d land max_int) mod (Array.length t.table);;

  let rec resize compare t =
    let oldlen = Array.length t.table in
    let newlen = next_sz oldlen in
    if newlen > oldlen then begin
      let newt = create newlen in
      newt.limit <- t.limit + 100;          (* prevent resizing of newt *)
      fold (fun d () -> add compare newt d) t ();
   (* assert Array.length newt.table = newlen; *)
      t.table <- newt.table;
   (* t.limit <- t.limit + 2; -- performance bug *)
    end

  and add_aux compare t d index =
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then begin
	let newsz = min (sz + 3) (Sys.max_array_length - 1) in
	if newsz <= sz then failwith "Weak.Make : hash bucket cannot grow more";
	let newbucket = Weak.create newsz in
	Weak.blit bucket 0 newbucket 0 sz;
	Weak.set newbucket i (Some d);
	t.table.(index) <- newbucket;
	t.totsize <- t.totsize + (newsz - sz);
	if t.totsize > t.limit * Array.length t.table then resize compare t;
      end else begin
	if Weak.check bucket i
	then loop (i+1)
	else Weak.set bucket i (Some d)
      end
    in
    loop 0;

  and add compare t d = add_aux compare t d (get_index compare t d)

  let find_or (compare:'a compare) t d ifnotfound =
    let index = get_index compare t d in
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then ifnotfound index
      else begin
	match Weak.get_copy bucket i with
	| Some v when compare.equal v d
	   -> begin match Weak.get bucket i with
	      | Some v -> v
	      | None -> loop (i+1)
	      end
	| _ -> loop (i+1)
      end
    in
    loop 0

  let merge (compare:'a compare) t d =
    find_or compare t d (fun index -> add_aux compare t d index; d)

  let find (compare:'a compare) t d =
    find_or compare t d (fun index -> raise Not_found)

  let find_shadow (compare:'a compare) t d iffound ifnotfound =
    let index = get_index compare t d in
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then ifnotfound else begin
	match Weak.get_copy bucket i with
	| Some v when compare.equal v d -> iffound bucket i
	| _ -> loop (i+1)
      end
    in
    loop 0

  let remove (compare:'a compare) t d =
    find_shadow compare t d (fun w i -> Weak.set w i None) ()

  let mem (compare:'a compare) t d =
    find_shadow compare t d (fun w i -> true) false

  let find_all compare t d =
    let index = get_index compare t d in
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i accu =
      if i >= sz then accu
      else begin
	match Weak.get_copy bucket i with
	| Some v when compare.equal v d
	   -> begin match Weak.get bucket i with
	      | Some v -> loop (i+1) (v::accu)
	      | None -> loop (i+1) accu
	      end
	| _ -> loop (i+1) accu
      end
    in
    loop 0 []

end

(** Weak hash tables *)

module type S = sig
  type data
  type t
  val create : int -> t
  val clear : t -> unit
  val merge : t -> data -> data
  val add : t -> data -> unit
  val remove : t -> data -> unit
  val find : t -> data -> data
  val find_all : t -> data -> data list
  val mem : t -> data -> bool
  val iter : (data -> unit) -> t -> unit
  val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a
  val count : t -> int
  val stats : t -> int * int * int * int * int * int
  val print :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    (Format.formatter -> data -> unit) ->
    Format.formatter -> t -> unit
end

module Make (H : Hashtbl.HashedType) : (S with type data = H.t
					  and type t = H.t hashtbl) =
struct
  type data = H.t
  type t = H.t hashtbl

  let compare = { hash = H.hash; equal = H.equal }

  let create = create
  let clear = clear
  let fold = fold
  let iter = iter
  let count = count
  let add = Compare.add compare
  let merge = Compare.merge compare
  let find = Compare.find compare
  let remove = Compare.remove compare
  let mem = Compare.mem compare
  let find_all = Compare.find_all compare
  let stats = stats
  let print = print
end

module Custom = struct
  type 'a t = {
    compare : 'a compare;
    hashtbl:'a hashtbl;
  }

  let create hash equal n = {
    compare = { hash=hash; equal=equal };
    hashtbl = create n
  }
  let clear t = clear t.hashtbl
  let fold f t init = fold f t.hashtbl init
  let iter f t = iter f t.hashtbl
  let count t = count t.hashtbl
  let add t data = Compare.add t.compare t.hashtbl data
  let merge t data = Compare.merge t.compare t.hashtbl data
  let find t data = Compare.find t.compare t.hashtbl data
  let remove t data = Compare.remove t.compare t.hashtbl data
  let mem t data = Compare.mem t.compare t.hashtbl data
  let find_all t data = Compare.find_all t.compare t.hashtbl data
  let stats t = stats t.hashtbl
  let print ?first ?sep ?last print_data fmt t = 
    print
      ?first ?sep ?last print_data fmt t.hashtbl
end


let compare = {
  hash=Hashtbl.hash;
  equal=(=)
}

let add t data = Compare.add compare t data
let merge t data = Compare.merge compare t data
let find t data = Compare.find compare t data
let remove t data = Compare.remove compare t data
let mem t data = Compare.mem compare t data
let find_all t data = Compare.find_all compare t data
