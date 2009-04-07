(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Hash tables *)

(* Modified by B. Jeannet: functions [map], [copy] and [print] *)

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

let hash x = hash_param 10 100 x

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) hashtbl = 
  { mutable size: int;                        (* number of elements *)
    mutable data: ('a, 'b) bucketlist array } (* the buckets *)

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist

type ('a,'b) t = ('a,'b) hashtbl

type 'a compare = {
  hash : 'a -> int;
  equal : 'a -> 'a -> bool;
}

let create initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0; data = Array.make s Empty }

let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- Empty
  done;
  h.size <- 0

let copy h =
  { size = h.size;
    data = Array.copy h.data }

let map f h =
  let rec map_bucketlist = function
    | Empty -> Empty
    | Cons(key,data,rest) -> Cons(key, f key data, map_bucketlist rest)
  in
  { size = h.size;
    data = Array.map map_bucketlist h.data }

let length h = h.size

let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(k, d, rest) ->
        f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons(k, d, rest) ->
        do_bucket rest (f k d accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

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
  (hash:('a,'b) t)
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
  let resize compare tbl =
    let odata = tbl.data in
    let osize = Array.length odata in
    let nsize = min (2 * osize + 1) Sys.max_array_length in
    if nsize <> osize then begin
      let ndata = Array.create nsize Empty in
      let rec insert_bucket = function
        | Empty -> ()
	| Cons(key, data, rest) ->
            insert_bucket rest; (* preserve original order of elements *)
            let nidx = (compare.hash key) mod nsize in
            ndata.(nidx) <- Cons(key, data, ndata.(nidx)) 
      in
      for i = 0 to osize - 1 do
	insert_bucket odata.(i)
      done;
      tbl.data <- ndata;
    end

  let add compare h key info =
    let i = (compare.hash key) mod (Array.length h.data) in
    let bucket = Cons(key, info, h.data.(i)) in
    h.data.(i) <- bucket;
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize compare h
      
  let remove compare h key =
    let rec remove_bucket = function
     | Empty ->
        Empty
    | Cons(k, i, next) ->
        if compare.equal k key
        then begin h.size <- pred h.size; next end
        else Cons(k, i, remove_bucket next) in
    let i = (compare.hash key) mod (Array.length h.data) in
    h.data.(i) <- remove_bucket h.data.(i)

  let rec find_rec compare key = function
    | Empty ->
	raise Not_found
    | Cons(k, d, rest) ->
	if compare.equal key k then d else find_rec compare key rest
	  
  let find compare h key =
    match h.data.((compare.hash key) mod (Array.length h.data)) with
    | Empty -> raise Not_found
    | Cons(k1, d1, rest1) ->
	if compare.equal key k1 then d1 else
	  match rest1 with
          | Empty -> raise Not_found
	  | Cons(k2, d2, rest2) ->
              if compare.equal key k2 then d2 else
		match rest2 with
		| Empty -> raise Not_found
		| Cons(k3, d3, rest3) ->
		    if compare.equal key k3 then d3 else find_rec compare key rest3
		      
  let find_all compare h key =
    let rec find_in_bucket = function
      | Empty ->
	  []
      | Cons(k, d, rest) ->
	  if compare.equal k key
	  then d :: find_in_bucket rest
	  else find_in_bucket rest in
    find_in_bucket h.data.((compare.hash key) mod (Array.length h.data))

  let replace compare h key info =
    let rec replace_bucket = function
      | Empty ->
          raise Not_found
      | Cons(k, i, next) ->
          if compare.equal k key
          then Cons(k, info, next)
          else Cons(k, i, replace_bucket next) in
    let i = (compare.hash key) mod (Array.length h.data) in
    let l = h.data.(i) in
    try
      h.data.(i) <- replace_bucket l
    with Not_found ->
      h.data.(i) <- Cons(key, info, l);
      h.size <- succ h.size;
      if h.size > Array.length h.data lsl 1 then resize compare h
	
  let mem compare h key =
    let rec mem_in_bucket = function
      | Empty ->
	  false
      | Cons(k, d, rest) ->
	  compare.equal k key || mem_in_bucket rest in
    mem_in_bucket h.data.((compare.hash key) mod (Array.length h.data))
end

let stdcompare = { hash = hash; equal = (=) }
let add h key info = Compare.add stdcompare h key info
let replace h key info = Compare.replace stdcompare h key info
let remove h key = Compare.remove stdcompare h key 
let find h key = Compare.find stdcompare h key 
let find_all h key = Compare.find_all stdcompare h key 
let mem h key = Compare.mem stdcompare h key 

(** Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type S =
  sig
    type key
    type 'a t = (key,'a) hashtbl

    module Hash : (HashedType with type t=key)

    val create: int -> 'a t
    val clear: 'a t -> unit
    val copy: 'a t -> 'a t
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_all: 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val map : (key -> 'a -> 'b) -> 'a t -> 'b t
    val length : 'a t -> int
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

module Make(H: HashedType): (S with type key = H.t and type 'a t = (H.t,'a) hashtbl) =
  struct
    type key = H.t
    type 'a t = (key,'a) hashtbl
    module Hash = H
    let create = create
    let clear = clear
    let copy = copy
    let map = map
    let iter = iter
    let fold = fold
    let length = length
    let print = print

    let compare = {
      hash = (fun key -> (H.hash key) land max_int);
      equal = H.equal
    }
    let add h key info = Compare.add compare h key info
    let replace h key info = Compare.replace compare h key info
    let remove h key = Compare.remove compare h key 
    let find h key = Compare.find compare h key 
    let find_all h key = Compare.find_all compare h key 
    let mem h key = Compare.mem compare h key 
end

