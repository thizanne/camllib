(* $Id: mappe.ml,v 1.10 2005/06/14 14:24:07 bjeannet Exp $ *)

(** Association tables over ordered types *)

type ('a,'b) tzz =
    Emptyzz
  | Nodezz of ('a,'b) tzz * 'a * 'b * ('a,'b) tzz * int

type ('a,'b) t = ('a,'b) tzz

let repr : ('a,'b) t -> ('a,'b) tzz = Obj.magic
let obj : ('a,'b) tzz -> ('a,'b) t = Obj.magic

let empty = Emptyzz

let height = function
    Emptyzz -> 0
  | Nodezz(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Nodezz(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let bal l x d r =
  let hl = match l with Emptyzz -> 0 | Nodezz(_,_,_,_,h) -> h in
  let hr = match r with Emptyzz -> 0 | Nodezz(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Emptyzz -> invalid_arg "Map.bal"
    | Nodezz(ll, lv, ld, lr, _) ->
	if height ll >= height lr then
	  create ll lv ld (create lr x d r)
	else begin
	  match lr with
	    Emptyzz -> invalid_arg "Map.bal"
	  | Nodezz(lrl, lrv, lrd, lrr, _)->
	      create (create ll lv ld lrl) lrv lrd (create lrr x d r)
	end
  end else if hr > hl + 2 then begin
    match r with
      Emptyzz -> invalid_arg "Map.bal"
    | Nodezz(rl, rv, rd, rr, _) ->
	if height rr >= height rl then
	  create (create l x d rl) rv rd rr
	else begin
	  match rl with
	    Emptyzz -> invalid_arg "Map.bal"
	  | Nodezz(rll, rlv, rld, rlr, _) ->
	      create (create l x d rll) rlv rld (create rlr rv rd rr)
	    end
  end else
    Nodezz(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let rec add x data = function
    Emptyzz ->
      Nodezz(Emptyzz, x, data, Emptyzz, 1)
  | Nodezz(l, v, d, r, h) ->
      let c = Pervasives.compare x v in
      if c = 0 then
	    Nodezz(l, x, data, r, h)
      else if c < 0 then
	    bal (add x data l) v d r
      else
	bal l v d (add x data r)

(** Same as create and bal, but no assumptions are made on the relative
     heights of l and r. *)
let rec join l x d r =
  match (l, r) with
    (Emptyzz, _) -> add x d r
  | (_, Emptyzz) -> add x d l
  | (Nodezz(ll, lx, ld, lr, lh), Nodezz(rl, rx, rd, rr, rh)) ->
      if lh > rh + 2 then bal ll lx ld (join lr x d r) else
      if rh > lh + 2 then bal (join l x d rl) rx rd rr else
      create l x d r

let rec find x = function
    Emptyzz ->
      raise Not_found
  | Nodezz(l, v, d, r, _) ->
      let c = Pervasives.compare x v in
      if c = 0 then d
      else find x (if c < 0 then l else r)

let rec mem x = function
    Emptyzz ->
      false
  | Nodezz(l, v, d, r, _) ->
      let c = Pervasives.compare x v in
      c = 0 || mem x (if c < 0 then l else r)

let rec min_binding = function
  Emptyzz -> raise Not_found
  | Nodezz(Emptyzz, x, d, r, _) -> (x, d)
  | Nodezz(l, x, d, r, _) -> min_binding l

let rec remove_min_binding = function
  Emptyzz -> invalid_arg "Map.remove_min_elt"
  | Nodezz(Emptyzz, x, d, r, _) -> r
  | Nodezz(l, x, d, r, _) -> bal (remove_min_binding l) x d r

let merge t1 t2 =
  match (t1, t2) with
  (Emptyzz, t) -> t
  | (t, Emptyzz) -> t
  | (_, _) ->
      let (x, d) = min_binding t2 in
      bal t1 x d (remove_min_binding t2)

(** Merge two trees l and r into one.  All elements of l must precede the
    elements of r.  No assumption on the heights of l and r. *)
let concat t1 t2 =
  match (t1, t2) with
    (Emptyzz, t) -> t
  | (t, Emptyzz) -> t
  | (_, _) ->
      let (x,d) = min_binding t2 in
      join t1 x d (remove_min_binding t2)

(** Splitting.  split x s returns a quadruple (l, present, r) where
   - l is the set of elements of s that are < x
   - r is the set of elements of s that are > x
   - present is false if s contains no element equal to x,
   or true if s contains an element equal to x. *)
let rec split key = function
    Emptyzz ->
      (Emptyzz, false, Emptyzz)
  | Nodezz(l, x, d, r, _) ->
      let c = Pervasives.compare key x in
      if c = 0 then (l, true, r)
      else if c < 0 then
	let (ll, pres, rl) = split key l in (ll, pres, join rl x d r)
      else
	let (lr, pres, rr) = split key r in (join l x d lr, pres, rr)

let rec remove x = function
    Emptyzz ->
      Emptyzz
  | Nodezz(l, v, d, r, h) ->
      let c = Pervasives.compare x v in
      if c = 0 then
	merge l r
      else if c < 0 then
	bal (remove x l) v d r
      else
	bal l v d (remove x r)

let rec addmap m1 m2 =
  match (m1, m2) with
    (Emptyzz, t2) -> t2
  | (t1, Emptyzz) -> t1
  | (Nodezz(l1, x1, d1, r1, h1), Nodezz(l2, x2, d2, r2, h2)) ->
      if h1 >= h2 then
	if h2 = 1 then add x2 d2 m1 else begin
	  let (l2, _, r2) = split x1 m2 in
	  join (addmap l1 l2) x1 d1 (addmap r1 r2)
	end
      else
	if h1 = 1 then add x1 d1 m2 else begin
	  let (l1, _, r1) = split x2 m1 in
	  join (addmap l1 l2) x2 d2 (addmap r1 r2)
	end

let interset m1 s2 =
  let rec interset m1 s2 =
    match (m1, s2) with
    | (Emptyzz, t2) -> Emptyzz
    | (t1, Sette.Emptyzz) -> Emptyzz
    | (Nodezz(l1, x1, d1, r1, _), t2) ->
	match Sette.splitzz x1 t2 with
	| (l2, false, r2) ->
	    concat (interset l1 l2) (interset r1 r2)
	| (l2, true, r2) ->
	    join (interset l1 l2) x1 d1 (interset r1 r2)
  in
  interset m1 (Sette.repr s2)

let diffset m1 s2 =
  let rec diffset m1 s2 =
    match (m1, s2) with
    (Emptyzz, t2) -> Emptyzz
    | (t1, Sette.Emptyzz) -> t1
    | (Nodezz(l1, x1, d1, r1, _), t2) ->
	match Sette.splitzz x1 t2 with
	(l2, false, r2) ->
	  join (diffset l1 l2) x1 d1 (diffset r1 r2)
      | (l2, true, r2) ->
	  concat (diffset l1 l2) (diffset r1 r2)
  in
  diffset m1 (Sette.repr s2)

let rec iter f = function
    Emptyzz -> ()
  | Nodezz(l, v, d, r, _) ->
      iter f l; f v d; iter f r

let rec map f = function
    Emptyzz               -> Emptyzz
  | Nodezz(l, v, d, r, h) -> Nodezz(map f l, v, f d, map f r, h)

let maptoset map =
  let rec walk = function
    | Emptyzz               -> Sette.Emptyzz
    | Nodezz(l, v, d, r, h) -> Sette.Nodezz(walk l, v, walk r, h)
  in
  Sette.obj (walk (repr map))

let mapofset f set =
  let rec walk = function
    | Sette.Emptyzz -> Emptyzz
    | Sette.Nodezz(l,v,r,h) -> Nodezz(walk l,v,f v,walk r,h)
  in
  obj (walk (Sette.repr set))

let rec mapi f = function
    Emptyzz               -> Emptyzz
  | Nodezz(l, v, d, r, h) -> Nodezz(mapi f l, v, f v d, mapi f r, h)

let rec fold f m accu =
  match m with
    Emptyzz -> accu
  | Nodezz(l, v, d, r, _) ->
      fold f l (f v d (fold f r accu))

let filter p m =
  let rec filt accu = function
    | Emptyzz -> accu
    | Nodezz(l, v, d, r, _) ->
	filt (filt (if p v d then add v d accu else accu) l) r in
  filt Emptyzz m

let partition p m =
  let rec part (t, f as accu) = function
    | Emptyzz -> accu
    | Nodezz(l, v, d, r, _) ->
	part (part (if p v d then (add v d t, f) else (t, add v d f)) l) r in
  part (Emptyzz, Emptyzz) m

let rec cardinal = function
    Emptyzz -> 0
  | Nodezz(l, v, d, r, _) -> cardinal l + 1 + cardinal r

type ('a,'b) enumeration = End | More of 'a * 'b * ('a,'b) t * ('a,'b) enumeration

let rec cons_enum m e =
  match m with
  | Emptyzz -> e
  | Nodezz(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

let compare cmp m1 m2 =
  let rec compare_aux e1 e2 =
    match (e1, e2) with
    | (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
	let c = Pervasives.compare v1 v2 in
	if c <> 0 then c else
	  let c = cmp d1 d2 in
	  if c <> 0 then c else
	    compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in
  compare_aux (cons_enum m1 End) (cons_enum m2 End)

let equal cmp m1 m2 =
  let rec equal_aux e1 e2 =
    match (e1, e2) with
    (End, End) -> true
    | (End, _)  -> false
    | (_, End) -> false
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
	Pervasives.compare v1 v2 = 0 && cmp d1 d2 &&
    equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in
  equal_aux (cons_enum m1 End) (cons_enum m2 End)

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
  (map:('a,'b) t)
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
    map;
  Format.fprintf formatter last

(** Output signature of the functor {!Mappe.Make}. *)
module type S = sig
  module Setkey : Sette.S
  type key = Setkey.Ord.t
  type 'a t

  val repr : 'a t -> (key,'a) tzz
  val obj : (key,'a) tzz -> 'a t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val addmap : 'a t -> 'a t -> 'a t
  val interset : 'a t -> Setkey.t -> 'a t
  val diffset : 'a t -> Setkey.t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val maptoset : 'a t -> Setkey.t
  val mapofset: (key -> 'a) -> Setkey.t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
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

(** Functor building an implementation of the map structure
   given a totally ordered type. *)
module Make(Setkey : Sette.S) = struct
  type key = Setkey.elt
  module Setkey=Setkey

  type 'a t = (key,'a) tzz

  let repr : 'a t -> (key,'a) tzz = Obj.magic
  let obj : (key,'a) tzz -> 'a t = Obj.magic


  let empty : 'a t = empty

  let rec add x data = function
    | Emptyzz ->
	Nodezz(Emptyzz, x, data, Emptyzz, 1)
    | Nodezz(l, v, d, r, h) ->
	let c = Setkey.Ord.compare x v in
	if c = 0 then
	  Nodezz(l, x, data, r, h)
	else if c < 0 then
	  bal (add x data l) v d r
	else
	  bal l v d (add x data r)

  (** Same as create and bal, but no assumptions are made on the relative
    heights of l and r. *)
  let rec join l x d r =
    match (l, r) with
    (Emptyzz, _) -> add x d r
    | (_, Emptyzz) -> add x d l
    | (Nodezz(ll, lx, ld, lr, lh), Nodezz(rl, rx, rd, rr, rh)) ->
	if lh > rh + 2 then bal ll lx ld (join lr x d r) else
	if rh > lh + 2 then bal (join l x d rl) rx rd rr else
	create l x d r

  (** Merge two trees l and r into one.  All elements of l must precede the
    elements of r.  No assumption on the heights of l and r. *)
  let concat t1 t2 =
    match (t1, t2) with
    (Emptyzz, t) -> t
    | (t, Emptyzz) -> t
    | (_, _) ->
	let (x,d) = min_binding t2 in
	join t1 x d (remove_min_binding t2)

  (** Splitting.  split x s returns a quadruple (l, present, r) where
    - l is the set of elements of s that are < x
    - r is the set of elements of s that are > x
    - present is false if s contains no element equal to x,
    or true if s contains an element equal to x. *)
  let rec split key = function
    Emptyzz ->
      (Emptyzz, false, Emptyzz)
    | Nodezz(l, x, d, r, _) ->
	let c = Setkey.Ord.compare key x in
	if c = 0 then (l, true, r)
	else if c < 0 then
	  let (ll, pres, rl) = split key l in (ll, pres, join rl x d r)
	else
	  let (lr, pres, rr) = split key r in (join l x d lr, pres, rr)

  let rec find x = function
    | Emptyzz ->
	raise Not_found
    | Nodezz(l, v, d, r, _) ->
	let c = Setkey.Ord.compare x v in
	if c = 0 then d
	else find x (if c < 0 then l else r)

  let rec mem x = function
    | Emptyzz ->
	false
    | Nodezz(l, v, d, r, _) ->
	let c = Setkey.Ord.compare x v in
	c = 0 || mem x (if c < 0 then l else r)

  let rec remove x = function
    | Emptyzz ->
	Emptyzz
    | Nodezz(l, v, d, r, h) ->
	let c = Setkey.Ord.compare x v in
	if c = 0 then
	  merge l r
	else if c < 0 then
	  bal (remove x l) v d r
	else
	  bal l v d (remove x r)

  let rec addmap m1 m2 =
    match (m1, m2) with
    | (Emptyzz, t2) -> t2
    | (t1, Emptyzz) -> t1
    | (Nodezz(l1, x1, d1, r1, h1), Nodezz(l2, x2, d2, r2, h2)) ->
	if h1 >= h2 then
	  if h2 = 1 then add x2 d2 m1 else begin
	    let (l2, _, r2) = split x1 m2 in
	    join (addmap l1 l2) x1 d1 (addmap r1 r2)
	  end
	else
	  if h1 = 1 then add x1 d1 m2 else begin
	    let (l1, _, r1) = split x2 m1 in
	    join (addmap l1 l2) x2 d2 (addmap r1 r2)
	  end

  let interset m1 s2 =
    let rec interset m1 s2 =
      match (m1, s2) with
      | (Emptyzz, t2) -> Emptyzz
      | (t1, Sette.Emptyzz) -> Emptyzz
      | (Nodezz(l1, x1, d1, r1, _), t2) ->
	  match Setkey.splitzz x1 t2 with
	  | (l2, false, r2) ->
	      concat (interset l1 l2) (interset r1 r2)
	  | (l2, true, r2) ->
	      join (interset l1 l2) x1 d1 (interset r1 r2)
    in
    interset m1 (Setkey.repr s2)

  let diffset m1 s2 =
    let rec diffset m1 s2 =
      match (m1, s2) with
      | (Emptyzz, t2) -> Emptyzz
      | (t1, Sette.Emptyzz) -> t1
      | (Nodezz(l1, x1, d1, r1, _), t2) ->
	  match Setkey.splitzz x1 t2 with
	  (l2, false, r2) ->
	    join (diffset l1 l2) x1 d1 (diffset r1 r2)
	  | (l2, true, r2) ->
	      concat (diffset l1 l2) (diffset r1 r2)
    in
    diffset m1 (Setkey.repr s2)

  let iter = iter
  let map = map
  let maptoset x = Setkey.obj (Sette.repr (maptoset (repr x)))
  let mapofset f x = mapofset f (Sette.obj (Setkey.repr x))

  let mapi = mapi
  let fold = fold

  let filter p m =
    let rec filt accu = function
      | Emptyzz -> accu
      | Nodezz(l, v, d, r, _) ->
	  filt (filt (if p v d then add v d accu else accu) l) r in
    filt Emptyzz m

  let partition p m =
    let rec part (t, f as accu) = function
      | Emptyzz -> accu
      | Nodezz(l, v, d, r, _) ->
	  part (part (if p v d then (add v d t, f) else (t, add v d f)) l) r in
    part (Emptyzz, Emptyzz) m

  let cardinal = cardinal

  type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

  let rec cons_enum m e =
    match m with
    | Emptyzz -> e
    | Nodezz(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

  let compare cmp m1 m2 =
    let rec compare_aux e1 e2 =
      match (e1, e2) with
      | (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
	let c = Setkey.Ord.compare v1 v2 in
	if c <> 0 then c else
	  let c = cmp d1 d2 in
	  if c <> 0 then c else
	    compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in
    compare_aux (cons_enum m1 End) (cons_enum m2 End)
      
  let equal cmp m1 m2 =
    let rec equal_aux e1 e2 =
      match (e1, e2) with
      (End, End) -> true
      | (End, _)  -> false
      | (_, End) -> false
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
	  Setkey.Ord.compare v1 v2 = 0 && cmp d1 d2 &&
      equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in
    equal_aux (cons_enum m1 End) (cons_enum m2 End)

  let print = print
end
