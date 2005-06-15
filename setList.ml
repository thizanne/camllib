(*
        setList.ml

        Ensembles implantes avec des listes triees
*)

(*
 *      $Log: setList.ml,v $
 *      Revision 1.6  2005/06/14 14:25:14  bjeannet
 *      Use now Print module.
 *
 *      Revision 1.5  2003/11/12 20:39:02  bjeannet
 *      OK for ocamldoc.
 *
 *      Revision 1.4  2003/11/12 17:26:00  bjeannet
 *      Mappe and Sette changed according to the OCaml 3.07 standard library.
 *      Printing functions generalized with optional arguments.
 *      Miscellaneous.
 *
 *      Revision 1.3  2003/02/12 09:56:38  bjeannet
 *      Added functions exists and for_all.
 *
 *      Revision 1.2  2001/07/12 15:37:25  bjeannet
 *      Corrected the type of SetList.fold, to be conforming to Set.fold.
 *
 *      Revision 1.1.1.1  2001/01/25 08:44:10  bjeannet
 *      Library of Caml modules dealing with sets, multisets, graphs, nested
 *      lists (Ilist), additionnal functions to standard module list (Listc),
 *      and a polymorphic version of standard module Map.
 *
 *
 *      Revision 1.1  1998/12/04 16:11:13  bjeannet
 *      Initial revision
 *
 *)
(** Sets over totally ordered type with lists *)

(* version generique *)

type 'a t = 'a list

let print = Print.list
let empty = []
let is_empty t = (t=[])
let rec mem elt = function
  | [] -> false
  | x::l ->
      let drp = compare x elt in
      if drp > 0 then false
      else if drp=0 then true
      else mem elt l
let of_list l = l
let to_list l = l
let singleton elt = [elt]
let rec add elt = function
  | [] -> [elt]
  | (x::l) as t ->
      let drp = compare x elt in
      if drp > 0 then elt::t
      else if drp=0 then t
      else x::(add elt l)
let rec remove elt = function
  | [] -> []
  | (x::l) as t ->
      let drp = compare x elt in
      if drp < 0 then x::(remove elt l)
      else if drp=0 then l
      else t
let rec union ta tb = match (ta,tb) with
| ([],_) -> tb
| (_,[]) -> ta
| (xa::la,xb::lb) ->
    let drp = compare xa xb in
    if drp > 0 then xb::(union ta lb)
    else if drp = 0 then xa::(union la lb)
    else xa::(union la tb)
let rec inter ta tb = match (ta,tb) with
| ([],_) | (_,[]) -> []
| (xa::la,xb::lb) ->
    let drp = compare xa xb in
    if drp > 0 then inter ta lb
    else if drp = 0 then xa::(inter la lb)
    else inter la tb
let rec diff ta tb = match (ta,tb) with
| ([],_) -> []
| (_,[]) -> ta
| (xa::la,xb::lb) ->
    let drp = compare xa xb in
    if drp > 0 then diff ta lb
    else if drp = 0 then diff la lb
    else xa :: (diff la tb)
let rec equal ta tb =  match (ta,tb) with
| ([],[]) -> true
| ([],_) | (_,[]) -> false
| (xa::la,xb::lb) -> if compare xa xb = 0 then equal la lb else false
let rec subset ta tb =  match (ta,tb) with
| ([],_) -> true
| (_,[]) -> false
| (xa::la,xb::lb) -> 
    let drp = compare xa xb in
    if drp > 0 then subset ta lb
    else if drp = 0 then subset la lb
    else false
let iter = List.iter
let rec fold f l accu =
  match l with
  | [] -> accu
  | a::l -> fold f l (f a accu)
let fold_right = List.fold_right
let fold_left = List.fold_left
let cardinal = List.length
let elements t = t
let min_elt = function
  | [] -> raise Not_found
  | x :: _ -> x
let rec max_elt = function
  | [] -> raise Not_found
  | [x] -> x
  | _ ::l -> max_elt l
let choose = min_elt
let compare =
  let rec cmp ta tb = match (ta,tb) with
  | ([],[]) -> 0
  | ([],_) -> -1
  | (_,[]) -> 1
  | (xa::la,xb::lb) ->
      let drp = compare xa xb in
      if drp = 0 then cmp la lb
      else drp
  in cmp
let rec filter f = function
  | [] -> []
  | x :: l -> 
      let suite = filter f l in
      if f x then x :: suite else suite
let partition f t =
  List.fold_right
    (begin fun x (a,b) ->
      if f x then
	(x :: a, b)
      else
	(a, x :: b)
    end)
    t ([],[])
let rec exists f = function
  | [] -> false
  | x :: l -> (f x) || (exists f l)
let rec for_all f = function
  | [] -> true
  | x :: l -> (f x) && (exists f l)

(* version parametree *)

(** Output signature of the functor {!SetList.Make} *)
module type S = sig
  include SetList.S
end

(** Functor building an implementation of the SetList structure
   given a totally ordered type. *)
module Make(Ord: Set.OrderedType) = struct
  type elt = Ord.t
  type t = elt list
  let print = print
  let empty = empty
  let is_empty = is_empty
  let rec mem elt = function
    | [] -> false
    | x::l ->
	let drp = Ord.compare x elt in
	if drp > 0 then false
	else if drp=0 then true
	else mem elt l
  let of_list = of_list
  let to_list = to_list
  let singleton = singleton
  let rec add elt = function
    | [] -> [elt]
    | (x::l) as t ->
	let drp = Ord.compare x elt in
	if drp > 0 then elt::t
	else if drp=0 then t
	else x::(add elt l)
  let rec remove elt = function
    | [] -> []
    | (x::l) as t ->
	let drp = Ord.compare x elt in
	if drp < 0 then x::(remove elt l)
	else if drp=0 then l
	else t
  let rec union ta tb = match (ta,tb) with
  | ([],_) -> tb
  | (_,[]) -> ta
  | (xa::la,xb::lb) ->
      let drp = Ord.compare xa xb in
      if drp > 0 then xb::(union ta lb)
      else if drp = 0 then xa::(union la lb)
      else xa::(union la tb)
  let rec inter ta tb = match (ta,tb) with
  | ([],_) | (_,[]) -> []
  | (xa::la,xb::lb) ->
      let drp = Ord.compare xa xb in
      if drp > 0 then inter ta lb
      else if drp = 0 then xa::(inter la lb)
      else inter la tb
  let rec diff ta tb = match (ta,tb) with
  | ([],_) -> []
  | (_,[]) -> ta
  | (xa::la,xb::lb) ->
      let drp = Ord.compare xa xb in
      if drp > 0 then diff ta lb
      else if drp = 0 then diff la lb
      else xa :: (diff la tb)
  let rec equal ta tb =  match (ta,tb) with
  | ([],[]) -> true
  | ([],_) | (_,[]) -> false
  | (xa::la,xb::lb) -> if Ord.compare xa xb = 0 then equal la lb else false
  let rec subset ta tb =  match (ta,tb) with
  | ([],_) -> true
  | (_,[]) -> false
  | (xa::la,xb::lb) -> 
      let drp = Ord.compare xa xb in
      if drp > 0 then subset ta lb
      else if drp = 0 then subset la lb
      else false
  let iter = List.iter
  let fold = fold
  let fold_right = List.fold_right
  let fold_left = List.fold_left
  let cardinal = List.length
  let elements t = t
  let min_elt = min_elt
  let max_elt = max_elt
  let choose = choose
  let rec compare ta tb = match (ta,tb) with
  | ([],[]) -> 0
  | ([],_) -> -1
  | (_,[]) -> 1
  | (xa::la,xb::lb) ->
      let drp = Ord.compare xa xb in
      if drp = 0 then compare la lb
      else drp
  let filter = filter
  let partition = partition
  let exists = exists
  let for_all = for_all
end
