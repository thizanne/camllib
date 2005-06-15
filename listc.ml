(*i
 * $Id: listc.ml,v 1.3 2003/11/12 20:39:02 bjeannet Exp $
 *
 * Complementary functions on lists
 *
i*)

(** Complementary functions on lists *)

(* 
   [val rg : 'a -> 'a list -> int = <fun>]

   Rang d'un élément dans une liste, en commencant a 0
   Lance l'exception [Not_found] si l'élément n'appartient pas à la liste
*)

let rg el liste = 
  let rec parcours n = function
      [] -> raise Not_found
    | x::l -> if x=el then n else (parcours (n+1) l)
  in
  (parcours 0 liste)

(*
  [val split_head_last : 'a list -> 'a list * 'a]

  [split_head_last [a1;...;an] = ([a1;...;an-1],an)].
  Lance [Failure] si la liste en entree est vide.
*)

let rec split_head_last = function
  [] -> failwith "Listc.split_head_last"
| [x] -> ([],x)
| x::l -> let (head,last) = split_head_last l in (x::head,last)

(*
  [val pick : ('a -> bool) -> 'a list -> 'a]

  pick predicat entree = retourne le premier element verifiant predicat; 
  s'il n'y en a pas, exception [Not_found]
*)
let rec pick predicat = function
  | [] -> raise Not_found
  | x::l -> if (predicat x) then x else (pick predicat l)

(*
  rem : 'a -> 'a list -> 'a list
  remq : 'a -> 'a list -> 'a list

  (remove x l) : on retounre l moins le premier element x trouve 
  rem : egalite structurelle
  remq : egalite physique
*)

let rec rem e = function
    [] -> []
  | x::l -> if x=e then l else x :: (rem e l)
let rec remq e = function
    [] -> []
  | x::l -> if x==e then l else x :: (rem e l)

let rec remp predicat = function
    [] -> []
  | x::l -> if predicat x then l else x :: (remp predicat l)

(*
  remove_duplicatas : 'a list -> 'a list

  enleve les doublons (au sens de (=))
*)

let remove_duplicatas = function
    [] -> []
  | x::l ->
      let rec parcours e = function
	  [] -> []
	| x::l as liste -> 
	    let res = (parcours x l) in
	    if (List.mem e liste) then res else e::res
      in
      parcours x l

(*
  append_without_duplicatas : 'a list -> 'a list -> 'a list

  prend 2 listes sans duplicatas (au sens de (=)) et rend la concatenation
  en enlevant les duplicatas
*)

let rec append_without_duplicatas a b =
  match a with
    [] -> b
  | ha::ta ->
      let res = (append_without_duplicatas ta b) in
      if (List.mem ha b) then res else ha::res


(*
  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  val iteri : (int -> 'a -> unit) -> 'a list -> unit

  (mapi f [a0;...;an]) = [f 0 a0; ...; f n an]
  (iteri f [a0;...;an]) = f 0 a0; ...; f n an
*)

let mapi f l =
  let rec parcours n = function
      [] -> []
    | x::l -> (f n x) :: (parcours (n+1) l)
  in (parcours 0 l)

let iteri (f : int -> 'a -> unit) l =
  let rec parcours n = function
      [] -> ()
    | x::l -> (f n x); (parcours (n+1) l)
  in (parcours 0 l)

let foldi f l res =
  let rec parcours n res = function
    | [] -> res
    | x::l -> parcours (n+1) (f n x res) l
  in parcours 0 res l
(*
  val distribuer_operation :
    ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list = <fun>

  distribuer_operation f [a1;a2;...;an] [b1;b2;...;bn] = 
    [f a1 b1; f a1 b2;...;f a2 b1; ... ; f an bn]
*)
let distribuer_operation f la lb =
  let rec dist2 res x lb = match lb with
    [] -> res
  | y::l -> let nres = (f x y)::res in (dist2 nres x l)
  in
  let rec dist1 res la = match la with
    [] -> res
  | x::l -> let nres = (dist2 res x lb) in (dist1 nres l)
  in
  (dist1 [] la)
	   
(* 
   val inv_assoc : 'a -> ('b * 'a) list -> 'b 
   val inv_mem_assoc : 'a -> ('b * 'a) list -> bool
   val inv_assq : 'a -> ('b * 'a) list -> 'b
   association inverse 
*)

let rec inv_assoc cle = function 
    [] -> raise Not_found
  | (a,b)::reste ->
      if b=cle then a else inv_assoc cle reste

let rec inv_mem_assoc cle = 
  function 
    [] -> false
  | (a,b)::reste ->
      if b=cle then true else inv_mem_assoc cle reste

let rec inv_assq cle = function 
    [] -> raise Not_found
  | (a,b)::reste ->
      if b==cle then a else inv_assq cle reste

(* 
   val assoc_create : 'a -> 'b -> ('a * 'b) list ref -> 'b 
   association avec creation 
*)

let assoc_create cle nres liste =
  try
    List.assoc cle !liste
  with Not_found ->
    liste := (cle,nres)::(!liste); nres

(* pretty  printing *)
let pretty_print func liste = 
  Format.printf "@[<1>[";
  let rec do_sep = function 
    | [e] -> func e
    | e::l ->
	begin
	  func e ; 
	  Format.printf ";@ ";
	  do_sep l
	end
    | [] -> ()
  in 
  do_sep liste;
  Format.printf "]@]"

let pp_pretty_print func formatter liste = 
  Format.fprintf formatter "@[<1>[";
  let rec do_sep = function 
    | [e] -> func formatter e
    | e::l ->
	begin
	  Format.fprintf formatter "%a;@ " func e;
	  do_sep l
	end
    | [] -> ()
  in 
  do_sep liste;
  Format.fprintf formatter "]@]"

(* pretty  printing *)
let pp_pretty_print_generalized deb sep fin func formatter liste = 
  Format.fprintf formatter "@[%s" deb;
  let rec do_sep = function 
    | [e] -> func formatter e
    | e::l ->
	Format.fprintf formatter "%a%s@," func e sep;
	do_sep l
    | [] -> ()
  in
  do_sep liste;
  Format.fprintf formatter "%s@]" fin
    

