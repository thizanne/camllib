(*
  *      $Id: setArray.ml,v 1.3 2003/11/12 20:39:02 bjeannet Exp $
  *
  *      Ensembles implantes avec des tableaux tries
*)

(** Sets over totally ordered type with arrays *)

(* version generique *)

type 'a t = 'a array

(* partie privee *)
type rank = 
  | Old of int
  | New of int

let rank elt set start =
  let size = Array.length set in
  let pos = ref start and
    rank = ref (Old(-1)) in
  begin
    try
      while !pos < size do
	let drp = compare elt set.(!pos) in
	if drp < 0 then begin
	  rank := New(!pos); raise Exit
	end
	else if drp = 0 then begin
	  rank := Old(!pos); raise Exit
	end;
	incr pos
      done;
      rank := New(!pos)
    with Exit -> ()
  end;
  !rank


let print func formatter set =
  let size = Array.length set in
  Format.fprintf formatter "@[<1>[";
  for i=0 to size - 2 do
    Format.fprintf formatter "%a;@ " func set.(i)
  done;
  Format.fprintf formatter "%a]@]" func set.(size-1)
let empty = [||]
let is_empty set = (set=[||])
let mem elt set =
  match rank elt set 0 with
  | Old _ -> true
  | New _ -> false

let of_list l = Array.of_list l
let to_list l = Array.to_list l
let of_array l = Array.copy l
let to_array l = Array.copy l
let singleton elt = [|elt|]

let add elt set =
  match rank elt set 0 with
  | Old _ -> set
  | New pos -> 
      let size = Array.length set in
      let rset = Array.make (size+1) elt in
      if pos = 0 then
	Array.blit set 0 rset 1 size
      else if pos = size then
	Array.blit set 0 rset 0 size
      else begin
	Array.blit set 0 rset 0 (pos-1);
	Array.blit set pos rset (pos+1) (size - pos)
      end;
      rset

let remove elt set =
  match rank elt set 0 with
  | Old pos -> 
      let size = Array.length set in
      if pos=0 then
	Array.sub set 1 (size-1)
      else if pos=size then
	Array.sub set 0 (size-1)
      else
	let rset = Array.make (size-1) set.(0) in
	Array.blit set 0 rset 0 (pos-1);
	Array.blit set (pos+1) rset pos (size-1-pos);
	rset
  | New _ -> set

let union seta setb =
  if seta=[||] then setb
  else if setb=[||] then seta
  else begin
    let sa = Array.length seta and
      sb = Array.length setb and
      a = ref 0 and
      b = ref 0 and
      i = ref 0 
    in
    let rset = Array.make (sa+sb) seta.(0) in
    while !a < sa && !b < sb do
      let drp = compare seta.(!a) setb.(!b) in
      if drp=0 then begin 
	rset.(!i) <- seta.(!a); incr a; incr b 
      end else begin
	if drp < 0 then begin 
	  rset.(!i) <- seta.(!a); incr a 
	end else begin
	  rset.(!i) <- setb.(!b); incr b 
	end
      end;
      incr i
    done;
    if !i < (sa+sb) then
      Array.sub rset 0 !i
    else
      rset
  end

let inter seta setb =
  if seta=[||] || setb=[||] then [||]
  else begin
    let sa = Array.length seta and
      sb = Array.length setb and
      a = ref 0 and
      b = ref 0 and
      i = ref 0 
    in
    let rset = Array.make (min sa sb) seta.(0) in
    while !a < sa && !b < sb do
      let drp = compare seta.(!a) setb.(!b) in
      if drp=0 then begin 
	rset.(!i) <- seta.(!a); incr i; incr a; incr b 
      end else begin
	if drp < 0 then incr a else incr b 
      end;
    done;
    if !i < (Array.length rset) then
      Array.sub rset 0 !i
    else
      rset
  end
let diff seta setb =
  if seta=[||] || setb=[||] then seta
  else begin
    let sa = Array.length seta and
      sb = Array.length setb and
      a = ref 0 and
      b = ref 0 and
      i = ref 0 
    in
    let rset = Array.copy seta in
    while !a < sa && !b < sb do
      let drp = compare seta.(!a) setb.(!b) in
      if drp<0 then begin rset.(!i) <- seta.(!a); incr i; incr a end 
      else if drp=0 then begin incr a; incr b end
      else incr b;
    done;
    if !i < (Array.length rset) then
      Array.sub rset 0 !i
    else
      rset
  end
let equal seta setb =
  let sa = Array.length seta and
    sb = Array.length setb 
  in
  if (sa<>sb) then
    false
  else begin
    try
      for i=0 to sa-1 do
	if (compare seta.(i) setb.(i)) <> 0 then raise Exit;
      done;
      true
    with Exit -> 
      false
  end
let subset seta setb =
  let sa = Array.length seta and
    sb = Array.length setb 
  in
  if (sa>sb) then 
    false
  else if (sa=sb) then
    equal seta setb
  else begin  
    try
      let a = ref 0 and
	b = ref 0 
      in
      while !a < sa && !b < sb do
	let drp = compare seta.(!a) setb.(!b) in
	if drp<0 then raise Exit
	else if drp=0 then begin incr a; incr b end
	else (* drp>0 *) incr b;
      done;
      (!a = sa)
    with Exit ->
      false
  end
let iter = Array.iter
let fold =Array.fold_left
let fold_right = Array.fold_right
let fold_left = Array.fold_left
let cardinal = Array.length
let elements = Array.to_list
let min_elt set = 
  if set = [||] then
    raise Not_found
  else
    set.(0)
let max_elt set =
  if set = [||] then
    raise Not_found
  else
    set.((Array.length set) - 1)
let choose = min_elt
let compare seta setb =
  let sa = Array.length seta and
    sb = Array.length setb 
  in
  let s = min sa sb in
  let res = ref 0 in
  begin try
    for i=0 to s do
      let drp = compare seta.(i) setb.(i) in
      if drp <> 0 then begin res := drp; raise Exit end
    done;
    res := compare sa sb
  with Exit -> ()
  end;
  !res
let filter f set =
  let size = Array.length set and
    rset = Array.copy set 
  in
  let r = ref 0 in
  for s=0 to size-1 do
    if f set.(s) then begin rset.(!r) <- set.(s); incr r end
  done;
  if !r < size then
    Array.sub rset 0 !r
  else
    rset
let partition f set =
  if set=[||] then (set,set) 
  else begin
    let size = Array.length set in
    let rset = Array.make (2*size) set.(0) in
    let a = ref 0 and
      b = ref size 
    in
    for s=0 to size-1 do
      if f set.(s) then begin 
	rset.(!a) <- set.(s); incr a
      end else begin
	rset.(!b) <- set.(s); incr b
      end
    done;
    if !a = 0 then ([||],set)
    else if !a=size then (set,[||])
    else (Array.sub rset 0 !a, Array.sub rset size !b)
  end    

(* version parametree *)

(** Output signature of the functor {!SetArray.Make} *)
module type S = sig
  type elt
  type t
  val print : (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val of_list : elt list -> t
  val to_list : t -> elt list
  val of_array : elt array -> t
  val to_array : t -> elt array
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val max_elt : t -> elt
  val choose : t -> elt
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> (t * t)
end

(** Functor building an implementation of the SetArray structure
   given a totally ordered type. *)
module Make(Ord: Set.OrderedType) = struct
  type elt = Ord.t
  type t = elt array
  let rank elt set start =
    let size = Array.length set in
    let pos = ref start and
	rank = ref (Old(-1)) in
    begin
      try
	while !pos < size do
	  let drp = Ord.compare elt set.(!pos) in
	  if drp < 0 then begin
	    rank := New(!pos); raise Exit
	  end
	  else if drp = 0 then begin
	    rank := Old(!pos); raise Exit
	  end;
	  incr pos
	done;
	rank := New(!pos)
      with Exit -> ()
    end;
    !rank

  let print = print
  let empty = empty
  let is_empty = is_empty
  let mem elt set =
    match rank elt set 0 with
    | Old _ -> true
    | New _ -> false

  let of_list = of_list
  let to_list = to_list
  let of_array l = Array.copy l
  let to_array l = Array.copy l
  let singleton = singleton
  let add elt set =
    match rank elt set 0 with
    | Old _ -> set
    | New pos -> 
	let size = Array.length set in
	let rset = Array.make (size+1) elt in
	if pos = 0 then
	  Array.blit set 0 rset 1 size
	else if pos = size then
	  Array.blit set 0 rset 0 size
	else begin
	  Array.blit set 0 rset 0 (pos-1);
	  Array.blit set pos rset (pos+1) (size - pos)
	end;
	rset
	  
  let remove elt set =
    match rank elt set 0 with
    | Old pos -> 
	let size = Array.length set in
	if pos=0 then
	  Array.sub set 1 (size-1)
	else if pos=size then
	  Array.sub set 0 (size-1)
	else
	  let rset = Array.make (size-1) set.(0) in
	  Array.blit set 0 rset 0 (pos-1);
	  Array.blit set (pos+1) rset pos (size-1-pos);
	  rset
    | New _ -> set
	  
  let union seta setb =
    if seta=[||] then setb
    else if setb=[||] then seta
    else begin
      let sa = Array.length seta and
	  sb = Array.length setb and
	  a = ref 0 and
	  b = ref 0 and
	  i = ref 0 
      in
      let rset = Array.make (sa+sb) seta.(0) in
      while !a < sa && !b < sb do
	let drp = Ord.compare seta.(!a) setb.(!b) in
	if drp=0 then begin 
	  rset.(!i) <- seta.(!a); incr a; incr b 
	end else begin
	  if drp < 0 then begin 
	    rset.(!i) <- seta.(!a); incr a 
	  end else begin
	    rset.(!i) <- setb.(!b); incr b 
	  end
	end;
	incr i
      done;
      if !i < (sa+sb) then
	Array.sub rset 0 !i
      else
	rset
    end
	
  let inter seta setb =
    if seta=[||] || setb=[||] then [||]
    else begin
      let sa = Array.length seta and
	  sb = Array.length setb and
	  a = ref 0 and
	  b = ref 0 and
	  i = ref 0 
      in
      let rset = Array.make (min sa sb) seta.(0) in
      while !a < sa && !b < sb do
	let drp = Ord.compare seta.(!a) setb.(!b) in
	if drp=0 then begin 
	  rset.(!i) <- seta.(!a); incr i; incr a; incr b 
	end else begin
	  if drp < 0 then incr a else incr b 
	end;
      done;
      if !i < (Array.length rset) then
	Array.sub rset 0 !i
      else
	rset
    end
  let diff seta setb =
    if seta=[||] || setb=[||] then seta
    else begin
      let sa = Array.length seta and
	  sb = Array.length setb and
	  a = ref 0 and
	  b = ref 0 and
	  i = ref 0 
      in
      let rset = Array.copy seta in
      while !a < sa && !b < sb do
	let drp = Ord.compare seta.(!a) setb.(!b) in
	if drp<0 then begin rset.(!i) <- seta.(!a); incr i; incr a end 
	else if drp=0 then begin incr a; incr b end
	else incr b;
      done;
      if !i < (Array.length rset) then
	Array.sub rset 0 !i
      else
	rset
    end
  let equal seta setb =
    let sa = Array.length seta and
	sb = Array.length setb 
    in
    if (sa<>sb) then
      false
    else begin
      try
	for i=0 to sa-1 do
	  if (Ord.compare seta.(i) setb.(i)) <> 0 then raise Exit;
	done;
	true
      with Exit -> 
	false
    end
  let subset seta setb =
    let sa = Array.length seta and
	sb = Array.length setb 
    in
    if (sa>sb) then 
      false
    else if (sa=sb) then 
      equal seta setb
    else begin  
      try
	let a = ref 0 and
	    b = ref 0 
	in
	while !a < sa && !b < sb do
	  let drp = Ord.compare seta.(!a) setb.(!b) in
	  if drp<0 then raise Exit
	  else if drp=0 then begin incr a; incr b end
	  else (* drp>0 *) incr b;
	done;
	(!a = sa)
      with Exit ->
	false
    end
  let iter = Array.iter
  let fold = Array.fold_left
  let fold_right = Array.fold_right
  let fold_left = Array.fold_left
  let cardinal = Array.length
  let elements t = elements t
  let min_elt = min_elt
  let max_elt = max_elt
  let choose = choose
  let compare seta setb =
    let sa = Array.length seta and
	sb = Array.length setb 
    in
    let s = min sa sb in
    let res = ref 0 in
    begin try
      for i=0 to s do
	let drp = Ord.compare seta.(i) setb.(i) in
	if drp <> 0 then begin res := drp; raise Exit end;
      done;
      res := Pervasives.compare sa sb
    with Exit -> ()
    end;
    !res
  let filter = filter
  let partition = partition
end
