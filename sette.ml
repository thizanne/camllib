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

(* $Id: sette.ml,v 1.8 2005/06/14 14:26:00 bjeannet Exp $ *)

(** Sets over ordered types *)

(**  Modified by B. Jeannet to get a generic type and a few additions
    (like conversions form and to maps and pretty-printing). *)

(** Sets are represented by balanced binary trees (the heights of the children
    differ by at most 2 *)
type 'a tzz = 
  | Emptyzz 
  | Nodezz of ('a tzz) * 'a * ('a tzz) * int
type 'a t = 'a tzz

let repr : 'a t -> 'a tzz = Obj.magic
let obj : 'a tzz -> 'a t = Obj.magic

let height = function
  | Emptyzz -> 
0
  | Nodezz(_, _, _, h) -> h

(** Creates a new node with left son l, value v and right son r.  We must have
    all elements of l < v < all elements of r.  l and r must be balanced and |
    height l - height r | <= 2.  Inline expansion of height for better
    speed. *)
let create l v r =
  let hl = match l with Emptyzz -> 0 | Nodezz(_,_,_,h) -> h in
  let hr = match r with Emptyzz -> 0 | Nodezz(_,_,_,h) -> h in
  Nodezz(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

(** Same as create, but performs one step of rebalancing if necessary.
    Assumes l and r balanced and | height l - height r | <= 3.  Inline
    expansion of create for better speed in the most frequent case where no
    rebalancing is required. *)
let bal l v r =
  let hl = match l with Emptyzz -> 0 | Nodezz(_,_,_,h) -> h in
  let hr = match r with Emptyzz -> 0 | Nodezz(_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Emptyzz -> invalid_arg "Set.bal"
    | Nodezz(ll, lv, lr, _) ->
        if height ll >= height lr then
          create ll lv (create lr v r)
        else begin
          match lr with
            Emptyzz -> invalid_arg "Set.bal"
          | Nodezz(lrl, lrv, lrr, _)->
              create (create ll lv lrl) lrv (create lrr v r)
        end
  end else if hr > hl + 2 then begin
    match r with
      Emptyzz -> invalid_arg "Set.bal"
    | Nodezz(rl, rv, rr, _) ->
        if height rr >= height rl then
          create (create l v rl) rv rr
        else begin
          match rl with
            Emptyzz -> invalid_arg "Set.bal"
          | Nodezz(rll, rlv, rlr, _) ->
              create (create l v rll) rlv (create rlr rv rr)
        end
  end else
    Nodezz(l, v, r, (if hl >= hr then hl + 1 else hr + 1))
      
(** Insertion of one element *)
let rec add x = function
    Emptyzz -> Nodezz(Emptyzz, x, Emptyzz, 1)
  | Nodezz(l, v, r, _) as t ->
      let c = Pervasives.compare x v in
      if c = 0 then t else
      if c < 0 then bal (add x l) v r else bal l v (add x r)

 (** Same as create and bal, but no assumptions are made on the relative
     heights of l and r. *)
let rec join l v r =
  match (l, r) with
    (Emptyzz, _) -> add v r
  | (_, Emptyzz) -> add v l
  | (Nodezz(ll, lv, lr, lh), Nodezz(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      create l v r
	
(** Smallest and greatest element of a set *)
let rec min_elt = function
    Emptyzz -> raise Not_found
  | Nodezz(Emptyzz, v, r, _) -> v
  | Nodezz(l, v, r, _) -> min_elt l

let rec max_elt = function
    Emptyzz -> raise Not_found
  | Nodezz(l, v, Emptyzz, _) -> v
  | Nodezz(l, v, r, _) -> max_elt r

(** Remove the smallest element of the given set *)
let rec remove_min_elt = function
    Emptyzz -> invalid_arg "Set.remove_min_elt"
  | Nodezz(Emptyzz, v, r, _) -> r
  | Nodezz(l, v, r, _) -> bal (remove_min_elt l) v r

(** Merge two trees l and r into one.  All elements of l must precede the
    elements of r.  Assume | height l - height r | <= 2. *)
let merge t1 t2 =
  match (t1, t2) with
    (Emptyzz, t) -> t
  | (t, Emptyzz) -> t
  | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

(** Merge two trees l and r into one.  All elements of l must precede the
    elements of r.  No assumption on the heights of l and r. *)
let concat t1 t2 =
  match (t1, t2) with
    (Emptyzz, t) -> t
  | (t, Emptyzz) -> t
  | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

(** Splitting.  split x s returns a triple (l, present, r) where
   - l is the set of elements of s that are < x
   - r is the set of elements of s that are > x
   - present is false if s contains no element equal to x,
   or true if s contains an element equal to x. *)
let rec split x = function
    Emptyzz ->
      (Emptyzz, false, Emptyzz)
  | Nodezz(l, v, r, _) ->
      let c = Pervasives.compare x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split x l in (ll, pres, join rl v r)
      else
        let (lr, pres, rr) = split x r in (join l v lr, pres, rr)

(** Exported for [Mappe] *)
let splitzz = split

(** Implementation of the set operations *)

let empty = Emptyzz

let is_empty = function Emptyzz -> true | _ -> false

let rec mem x = function
    Emptyzz -> false
  | Nodezz(l, v, r, _) ->
      let c = Pervasives.compare x v in
      c = 0 || mem x (if c < 0 then l else r)

let singleton x = Nodezz(Emptyzz, x, Emptyzz, 1)

let rec remove x = function
    Emptyzz -> Emptyzz
  | Nodezz(l, v, r, _) ->
      let c = Pervasives.compare x v in
      if c = 0 then merge l r else
      if c < 0 then bal (remove x l) v r else bal l v (remove x r)

let rec union s1 s2 =
  match (s1, s2) with
    (Emptyzz, t2) -> t2
  | (t1, Emptyzz) -> t1
  | (Nodezz(l1, v1, r1, h1), Nodezz(l2, v2, r2, h2)) ->
      if h1 >= h2 then
        if h2 = 1 then add v2 s1 else begin
          let (l2, _, r2) = split v1 s2 in
          join (union l1 l2) v1 (union r1 r2)
        end
      else
        if h1 = 1 then add v1 s2 else begin
          let (l1, _, r1) = split v2 s1 in
          join (union l1 l2) v2 (union r1 r2)
        end

let rec inter s1 s2 =
  match (s1, s2) with
    (Emptyzz, t2) -> Emptyzz
  | (t1, Emptyzz) -> Emptyzz
  | (Nodezz(l1, v1, r1, _), t2) ->
      match split v1 t2 with
        (l2, false, r2) ->
          concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) ->
          join (inter l1 l2) v1 (inter r1 r2)

let rec diff s1 s2 =
  match (s1, s2) with
    (Emptyzz, t2) -> Emptyzz
  | (t1, Emptyzz) -> t1
  | (Nodezz(l1, v1, r1, _), t2) ->
      match split v1 t2 with
        (l2, false, r2) ->
          join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) ->
          concat (diff l1 l2) (diff r1 r2)

let rec compare_aux l1 l2 =
  match (l1, l2) with
    ([], []) -> 0
  | ([], _)  -> -1
  | (_, []) -> 1
  | (Emptyzz :: t1, Emptyzz :: t2) ->
      compare_aux t1 t2
  | (Nodezz(Emptyzz, v1, r1, _) :: t1, Nodezz(Emptyzz, v2, r2, _) :: t2) ->
      let c = Pervasives.compare v1 v2 in
      if c <> 0 then c else compare_aux (r1::t1) (r2::t2)
  | (Nodezz(l1, v1, r1, _) :: t1, t2) ->
      compare_aux (l1 :: Nodezz(Emptyzz, v1, r1, 0) :: t1) t2
  | (t1, Nodezz(l2, v2, r2, _) :: t2) ->
      compare_aux t1 (l2 :: Nodezz(Emptyzz, v2, r2, 0) :: t2)

let compare s1 s2 =
  compare_aux [s1] [s2]

let equal s1 s2 =
  compare s1 s2 = 0

let rec subset s1 s2 =
  match (s1, s2) with
    Emptyzz, _ ->
      true
  | _, Emptyzz ->
      false
  | Nodezz (l1, v1, r1, _), (Nodezz (l2, v2, r2, _) as t2) ->
      let c = Pervasives.compare v1 v2 in
      if c = 0 then
        subset l1 l2 && subset r1 r2
      else if c < 0 then
        subset (Nodezz (l1, v1, Emptyzz, 0)) l2 && subset r1 t2
      else
        subset (Nodezz (Emptyzz, v1, r1, 0)) r2 && subset l1 t2

let rec iter f = function
    Emptyzz -> ()
  | Nodezz(l, v, r, _) -> iter f l; f v; iter f r

let rec fold f s accu =
  match s with
    Emptyzz -> accu
  | Nodezz(l, v, r, _) -> fold f l (f v (fold f r accu))

let rec for_all p = function
    Emptyzz -> true
  | Nodezz(l, v, r, _) -> p v && for_all p l && for_all p r

let rec exists p = function
    Emptyzz -> false
  | Nodezz(l, v, r, _) -> p v || exists p l || exists p r

let filter p s =
  let rec filt accu = function
    | Emptyzz -> accu
    | Nodezz(l, v, r, _) ->
        filt (filt (if p v then add v accu else accu) l) r in
  filt Emptyzz s

let partition p s =
  let rec part (t, f as accu) = function
    | Emptyzz -> accu
    | Nodezz(l, v, r, _) ->
        part (part (if p v then (add v t, f) else (t, add v f)) l) r in
  part (Emptyzz, Emptyzz) s

let rec cardinal = function
    Emptyzz -> 0
  | Nodezz(l, v, r, _) -> cardinal l + 1 + cardinal r

let rec elements_aux accu = function
    Emptyzz -> accu
  | Nodezz(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

let elements s =
  elements_aux [] s

let choose = min_elt

let print 
  ?(first : (unit, Format.formatter, unit) format = ("[@[" : (unit, Format.formatter, unit) format))
  ?(sep : (unit, Format.formatter, unit) format = ("@ ":(unit, Format.formatter, unit) format))
  ?(last : (unit, Format.formatter, unit) format = ("@]]":(unit, Format.formatter, unit) format))
  (print_elt:Format.formatter -> 'a -> unit)
  (formatter:Format.formatter)
  (set:'a t) 
  : unit
  = 
  Format.fprintf formatter first;
  let firstelt = ref true in
  iter
    (begin fun elt ->
      if !firstelt then firstelt := false else Format.fprintf formatter sep;
      Format.fprintf formatter "%a" print_elt elt
    end)
    set;
  Format.fprintf formatter last

(** Output signature of the functor {!Sette.Make}. *)
module type S =
  sig
    type elt
    type t
    module Ord : (Set.OrderedType with type t=elt)
    val repr : t -> elt tzz
    val obj : elt tzz -> t
    val splitzz : elt -> elt tzz -> elt tzz * bool * elt tzz

    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> t -> t
    val singleton: elt -> t
    val remove: elt -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val subset: t -> t -> bool
    val iter: (elt -> unit) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all: (elt -> bool) -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val filter: (elt -> bool) -> t -> t
    val partition: (elt -> bool) -> t -> t * t
    val cardinal: t -> int
    val elements: t -> elt list
    val min_elt: t -> elt
    val max_elt: t -> elt
    val choose: t -> elt
    val print:
      ?first:(unit, Format.formatter, unit) format ->
      ?sep:(unit, Format.formatter, unit) format ->
      ?last:(unit, Format.formatter, unit) format ->
      (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
  end

(** Functor building an implementation of the set structure
   given a totally ordered type. *)
module Make(Ord: Set.OrderedType) =
  struct
    type elt = Ord.t
    type t = elt tzz
    module Ord = Ord

    let repr : t -> elt tzz = Obj.magic
    let obj : elt tzz -> t = Obj.magic

    (* Insertion of one element *)	
    let rec add x = function
        Emptyzz -> Nodezz(Emptyzz, x, Emptyzz, 1)
      | Nodezz(l, v, r, _) as t ->
          let c = Ord.compare x v in
          if c = 0 then t else
          if c < 0 then bal (add x l) v r else bal l v (add x r)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)
    let rec join l v r =
      match (l, r) with
        (Emptyzz, _) -> add v r
      | (_, Emptyzz) -> add v l
      | (Nodezz(ll, lv, lr, lh), Nodezz(rl, rv, rr, rh)) ->
          if lh > rh + 2 then bal ll lv (join lr v r) else
          if rh > lh + 2 then bal (join l v rl) rv rr else
          create l v r

    (* Smallest and greatest element of a set *)
    let min_elt = min_elt
    let max_elt = max_elt

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)
    let concat t1 t2 =
      match (t1, t2) with
        (Emptyzz, t) -> t
      | (t, Emptyzz) -> t
      | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

    (* Splitting.  split x s returns a triple (l, present, r) where
       - l is the set of elements of s that are < x
       - r is the set of elements of s that are > x
       - present is false if s contains no element equal to x,
       or true if s contains an element equal to x. *)
    let rec split x = function
        Emptyzz ->
          (Emptyzz, false, Emptyzz)
      | Nodezz(l, v, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then (l, true, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v r)
          else
            let (lr, pres, rr) = split x r in (join l v lr, pres, rr)
    (** Exported for [Mappe] *)
    let splitzz = split
	      
    (* Implementation of the set operations *)

    let empty = empty

    let is_empty = is_empty

    let rec mem x = function
        Emptyzz -> false
      | Nodezz(l, v, r, _) ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let singleton = singleton

    let rec remove x = function
        Emptyzz -> Emptyzz
      | Nodezz(l, v, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then merge l r else
          if c < 0 then bal (remove x l) v r else bal l v (remove x r)

    let rec union s1 s2 =
      match (s1, s2) with
        (Emptyzz, t2) -> t2
      | (t1, Emptyzz) -> t1
      | (Nodezz(l1, v1, r1, h1), Nodezz(l2, v2, r2, h2)) ->
          if h1 >= h2 then
            if h2 = 1 then add v2 s1 else begin
              let (l2, _, r2) = split v1 s2 in
              join (union l1 l2) v1 (union r1 r2)
            end
          else
            if h1 = 1 then add v1 s2 else begin
              let (l1, _, r1) = split v2 s1 in
              join (union l1 l2) v2 (union r1 r2)
            end

    let rec inter s1 s2 =
      match (s1, s2) with
        (Emptyzz, t2) -> Emptyzz
      | (t1, Emptyzz) -> Emptyzz
      | (Nodezz(l1, v1, r1, _), t2) ->
          match split v1 t2 with
            (l2, false, r2) ->
              concat (inter l1 l2) (inter r1 r2)
          | (l2, true, r2) ->
              join (inter l1 l2) v1 (inter r1 r2)

    let rec diff s1 s2 =
      match (s1, s2) with
        (Emptyzz, t2) -> Emptyzz
      | (t1, Emptyzz) -> t1
      | (Nodezz(l1, v1, r1, _), t2) ->
          match split v1 t2 with
            (l2, false, r2) ->
              join (diff l1 l2) v1 (diff r1 r2)
          | (l2, true, r2) ->
              concat (diff l1 l2) (diff r1 r2)

    let rec compare_aux l1 l2 =
        match (l1, l2) with
        ([], []) -> 0
      | ([], _)  -> -1
      | (_, []) -> 1
      | (Emptyzz :: t1, Emptyzz :: t2) ->
          compare_aux t1 t2
      | (Nodezz(Emptyzz, v1, r1, _) :: t1, Nodezz(Emptyzz, v2, r2, _) :: t2) ->
          let c = Ord.compare v1 v2 in
          if c <> 0 then c else compare_aux (r1::t1) (r2::t2)
      | (Nodezz(l1, v1, r1, _) :: t1, t2) ->
          compare_aux (l1 :: Nodezz(Emptyzz, v1, r1, 0) :: t1) t2
      | (t1, Nodezz(l2, v2, r2, _) :: t2) ->
          compare_aux t1 (l2 :: Nodezz(Emptyzz, v2, r2, 0) :: t2)

    let compare s1 s2 =
      compare_aux [s1] [s2]

    let equal s1 s2 =
      compare s1 s2 = 0

    let rec subset s1 s2 =
      match (s1, s2) with
        Emptyzz, _ ->
          true
      | _, Emptyzz ->
          false
      | Nodezz (l1, v1, r1, _), (Nodezz (l2, v2, r2, _) as t2) ->
          let c = Ord.compare v1 v2 in
          if c = 0 then
            subset l1 l2 && subset r1 r2
          else if c < 0 then
            subset (Nodezz (l1, v1, Emptyzz, 0)) l2 && subset r1 t2
          else
            subset (Nodezz (Emptyzz, v1, r1, 0)) r2 && subset l1 t2

    let iter =iter
    let fold = fold
    let for_all = for_all
    let exists = exists
    let filter = filter
    let partition = partition
    let cardinal = cardinal
    let elements = elements
    let min_elt = min_elt
    let max_elt = max_elt
    let choose = min_elt
    let print = print
  end
