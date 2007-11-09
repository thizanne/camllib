(* $Id$ *)

(* Bertrand Jeannet. This file is released under LGPL license. *)

(** Imbricated lists *)

type 'a el =
    Atome of 'a
  | List of 'a t
and 'a t =
    Nil
  | Cons of 'a el * 'a t

let cons x l = Cons(x,l)
and atome x = Atome(x)
and list l = List(l)

let hd = function
    Cons(x,_) -> x
  | _ -> raise (Failure "Ilist.hd")

let tl = function
    Cons(_,l) -> l
  | _ -> raise (Failure "Ilist.tl")

let rec length = function
    Nil -> 0
  | Cons(_,l) -> 1 + (length l)

let depth ilist =
  let rec depth_t = function
    | Nil -> 0
    | Cons(x,l) ->
	let dx = depth_elt x 
	and dl = depth_t l
	in
	max dx dl
  and depth_elt = function
    | Atome _ -> 0
    | List l -> 1 + depth_t l
  in
  if ilist=Nil then 0 else 1 + (depth_t ilist)
    
let rec append l1 l2 = match l1 with
  Cons(x,l) -> Cons(x,append l l2)
| Nil -> l2

let rec mem x = function
    Nil -> false
  | Cons(Atome(a),reste) -> (a=x) or mem x reste
  | Cons(List(l),reste) -> mem x l or mem x reste

let map f ilist = 
  let rec parcours flag = function
      Nil -> Nil
    | Cons(Atome(a),reste) -> 
	let fa = f flag a in 
	Cons(Atome(fa),parcours false reste)
    | Cons(List(l),reste) -> 
	let fl = parcours true l in 
	Cons(List(fl),parcours false reste)
  in
  parcours false ilist

let iter f ilist = 
  let rec parcours flag = function
    Nil -> ()
  | Cons(Atome(a),reste) -> f flag a; parcours false reste
  | Cons(List(l),reste) -> parcours true l; parcours false reste
  in
  parcours false ilist

let fold_left f res ilist =
  let rec parcours res flag = function
      Nil -> res
    | Cons(Atome(a),reste) -> 
	let nres = f res flag a in 
	parcours nres false reste
    | Cons(List(l),reste) -> 
	let nres = parcours res true l 
	in parcours nres false reste
  in
  parcours res false ilist

let fold_right f ilist res =
  let rec parcours res flag = function
      Nil -> res
    | Cons(Atome(a),reste) ->
	let nres = parcours res false reste in
	f flag a res
    | Cons(List(l),reste) -> 
	let nres = parcours res false reste in
	parcours nres true l 
  in
  parcours res false ilist


let rec rev_append l1 l2 =
  match l1 with
  | Nil -> l2
  | Cons(((Atome a) as atome),l) ->
      rev_append l (Cons(atome,l2))
  | Cons(List ilist,l) ->
      rev_append l (Cons(List(rev_append ilist Nil), l2))
      
let rev ilist = rev_append ilist Nil

let of_list list = 
  let ilist =
    List.fold_left 
      (fun res a -> Cons((Atome a),res))
      Nil list
  in
  rev ilist

let concat (ilist:'a t) : 'a list
  =
  let res =
    fold_left
      (fun (res:'a list) (flag:bool) (a:'a) -> a::res)
      [] ilist
  in
  List.rev res

let flatten ?(depth=1) (ilist:'a t) : 'a t
  =
  let rec rev_flatten res (cdepth:int) = function
    | Nil -> res
    | Cons(elt,rest) ->
	let nres = begin match elt with
	| Atome(a) ->
	    Cons(elt,res)
	| List(ilist) ->
	    if cdepth < depth then
	      Cons(
		List(rev_flatten Nil (cdepth+1) ilist),
		res
	      )
	    else
	      fold_left
		(fun res flag a -> Cons((Atome a),res))
		res ilist
	end
	in
	rev_flatten nres cdepth rest
  in
  let res = rev_flatten Nil 1 ilist in
  rev res

let iter_rec 
  (f:'a -> 'a Sette.t array -> unit)
  (ilist:'a t)
  =
  let set_of_ilist (ilist:'a t) : 'a Sette.t = 
    fold_left 
      (fun res flag x -> Sette.add x res) 
      Sette.empty ilist
  in
  let rec parcours (tset:'a Sette.t array) (ilist:'a t) : unit
    = 
    match ilist with
    | Nil -> ()
    | Cons(Atome(a),rest) ->
	let set = tset.(0) in
	tset.(0) <- Sette.remove a set;
	f a tset;
	tset.(0) <- set;
	parcours tset rest
    | Cons(List(l),rest) ->
	let set = tset.(0) in
	let setl = set_of_ilist l in
	tset.(0) <- Sette.diff set setl;
	let ntset = Array.append [|setl|] tset in
	parcours ntset l;
	tset.(0) <- set;
	parcours tset rest
  in
  let all = set_of_ilist ilist in
  parcours [|all|] ilist

let print
  ?(first : (unit, Format.formatter, unit) format = ("[@[" : (unit, Format.formatter, unit) format))
  ?(sep : (unit, Format.formatter, unit) format = (";@ ":(unit, Format.formatter, unit) format))
  ?(last : (unit, Format.formatter, unit) format = ("@]]":(unit, Format.formatter, unit) format))
  func formatter liste 
  =
  let rec printl liste = 
    Format.fprintf formatter first;
    let rec do_sep = function 
	Cons(e,Nil) -> printe e
      | Cons(e,l) -> printe e ; Format.fprintf formatter sep; do_sep l
      | Nil -> ()
    in 
    do_sep liste; Format.fprintf formatter last
  and printe = function
      Atome(x) -> func formatter x
    | List(l) -> printl l
  in
  printl liste
