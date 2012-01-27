(* Bertrand Jeannet. This file is released under LGPL license. *)

(** Imbricated lists *)

type 'a el =
    Atome of 'a
  | List of 'a t
and 'a t = 'a el list

let cons x l = x::l
and atome x = Atome(x)
and list l = List(l)

let hd = List.hd
let tl = List.tl
let length = List.length

let depth l =
  let rec depth maxdepth = function
    | [] -> maxdepth
    | (Atome _)::l -> depth maxdepth l
    | (List l2)::l ->
	depth (max maxdepth (1+(depth 1 l2))) l
  in
  depth 1 l

let append l1 l2 = l1 @ l2

let rec exists p = function
  | [] -> false
  | Atome(a)::l -> p a || exists p l
  | List(l2)::l -> exists p l2 || exists p l

let mem x l = exists (fun a -> x=a) l

let map f ilist =
  let rec parcours flag = function
    | [] -> []
    | Atome(a)::l ->
	Atome(f flag a)::(parcours false l)
    | List(l2)::l ->
	List(parcours true l2)::(parcours false l)
  in
  parcours false ilist

let iter f ilist =
  let rec parcours flag = function
    | [] -> ()
    | Atome(a)::l ->
	f flag a;
	parcours false l
    | List(l2)::l ->
	parcours true l2;
	parcours false l
  in
  parcours false ilist

let fold_left f res ilist =
  let rec parcours res flag = function
    | [] -> res
    | Atome(a)::l ->
	let nres = f res flag a in
	parcours nres false l
    | List(l2)::l ->
	let nres = parcours res true l2 in
	parcours nres false l
  in
  parcours res false ilist

let fold_right f ilist res =
  let rec parcours res flag = function
    | [] -> res
    | Atome(a)::l ->
	let nres = parcours res false l in
	f flag a nres
    | List(l2)::l ->
	let nres = parcours res false l in
	parcours nres true l2
  in
  parcours res false ilist

let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | (Atome(a) as x1)::l1 -> rev_append l1 (x1::l2)
  | List(l)::l1 -> rev_append l1 (List(rev_append l [])::l2)

let rev l = rev_append l []

let of_list list =
  let ilist =
    List.fold_left
      (fun res a -> Atome(a)::res)
      [] list
  in
  rev ilist

let to_list ilist =
  let list =
    fold_left
      (fun res flag elt -> elt::res)
      [] ilist
  in
  List.rev list

let concat (ilist:'a t) : 'a list
  =
  let res =
    fold_left
      (fun (res:'a list) (flag:bool) (a:'a) -> a::res)
      [] ilist
  in
  rev res

let flatten ?(depth=1) (ilist:'a t) : 'a t
  =
  let rec rev_flatten res (cdepth:int) = function
    | [] -> res
    | x::l ->
	let nres = begin match x with
	| Atome(a) -> x::res
	| List(l2) ->
	    if cdepth < depth then
	      let l2 = rev_flatten [] (cdepth+1) l2 in
	      List(l2)::res
	    else
	      fold_left
		(fun res flag a -> Atome(a)::res)
		res l2
	end
	in
	rev_flatten nres cdepth l
  in
  let res = rev_flatten [] 1 ilist in
  rev res

let print
  ?(first : (unit, Format.formatter, unit) format = ("[@[" : (unit, Format.formatter, unit) format))
  ?(sep : (unit, Format.formatter, unit) format = (";@ ":(unit, Format.formatter, unit) format))
  ?(last : (unit, Format.formatter, unit) format = ("@]]":(unit, Format.formatter, unit) format))
  func formatter liste
  =
  let rec printl liste =
    Format.fprintf formatter first;
    let rec do_sep = function
      | [e] -> printe e
      | e::l -> printe e ; Format.fprintf formatter sep; do_sep l
      | [] -> ()
    in
    do_sep liste; Format.fprintf formatter last
  and printe = function
      Atome(x) -> func formatter x
    | List(l) -> printl l
  in
  printl liste
