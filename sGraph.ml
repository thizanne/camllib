(*
 * $Id: sGraph.ml,v 1.2 2003/11/12 20:39:02 bjeannet Exp $
 *
 * Operations on directed graphs (one-way information maintained)
 *
 *)

(** Directed graphs, with one-way information maintained *)

open Format
open Ilist

(* version fonctionnelle *)

type ('a,'b) node = {
    succ: 'a Sette.t;
    etiq: 'b
  }
type ('a,'b,'c) t = {
    nodes: ('a, ('a,'b) node) Mappe.t;
    arcs: ('a*'a,'c) Mappe.t
  } 

let node g n = Mappe.find n g.nodes
let succ g n = (node g n).succ
let etiq g n = (node g n).etiq

let arete g arc = Mappe.find arc g.arcs
let empty = { nodes = Mappe.empty; arcs = Mappe.empty }
let size g = Mappe.fold (fun _ _ n -> n+1) g.nodes 0

let is_empty g = (g.nodes = Mappe.empty)
let is_vertex g i =
  try let _ = node g i in true
  with Not_found -> false
let is_edge g arc = 
  try let _ = Mappe.find arc g.arcs in true
  with Not_found -> false

let vertices g = Mappe.fold (fun v nv set -> Sette.add v set) g.nodes Sette.empty
let edges g = Mappe.fold (fun arc arete set -> Sette.add arc set) g.arcs Sette.empty

let map_vertex g f = {
  nodes = 
  Mappe.mapi
    (fun v nv -> {
      succ = nv.succ;
      etiq = f v nv.etiq
    })
    g.nodes;
  arcs = g.arcs
} 
let iter_vertex g f = Mappe.iter (fun v nv -> f v nv.etiq nv.succ) g.nodes
let fold_vertex g r f = Mappe.fold (fun v nv r -> f v nv.etiq nv.succ r) g.nodes r
let map_edge g f = {
  nodes = g.nodes;
  arcs = Mappe.mapi f g.arcs
} 
let iter_edge g f = Mappe.iter f g.arcs
let fold_edge g r f = Mappe.fold f g.arcs r

let pred g n = 
  fold_vertex g Sette.empty
    (fun v _ succ set -> if Sette.mem n succ then Sette.add v set else set)

let add_edge g ((a,b) as arc) arete = 
  try {
    nodes = begin
      if is_edge g arc then
	g.nodes
      else
	let na = node g a in
	Mappe.add a 
	  { succ = Sette.add b na.succ; etiq = na.etiq }
	  g.nodes
    end;
    arcs = Mappe.add arc arete g.arcs
  } 
  with Not_found -> failwith "SGraph.add_edge"

let remove_edge g ((a,b) as arc) = 
  try {
    nodes = begin
      let na = node g a in
      Mappe.add a 
	{ succ = Sette.remove b na.succ; etiq = na.etiq }
	g.nodes
    end;
    arcs = Mappe.remove arc g.arcs
  } 
  with Not_found -> failwith "SGraph.remove_edge"
      
let add_vertex g v etiq = 
  try
    let anode = Mappe.find v g.nodes in 
    { nodes = Mappe.add v { succ = anode.succ; etiq = etiq } g.nodes;
      arcs = g.arcs } 
  with Not_found -> 
    { nodes = Mappe.add v { succ = Sette.empty; etiq = etiq } g.nodes;
      arcs = g.arcs } 

let remove_vertex g v = 
  try
    let nv = node g v in 
    let g = 
      { nodes = Mappe.remove v g.nodes;
	arcs = 
	Sette.fold
	  (fun succ arcs -> Mappe.remove (v,succ) arcs)
	  nv.succ
	  g.arcs }
    in
    Sette.fold
      (fun p g -> remove_edge g (p,v))
      (pred g v)
      g
  with Not_found ->
    raise (Failure "SGraph.remove_vertex")

(* fonctions auxiliaires *)
let squelette make_etiq g =
  Mappe.map 
    (fun n -> { succ = n.succ; etiq = make_etiq() })
    g.nodes

let squelette_multi root make_etiq g sroot =
  Mappe.add 
    root
    { succ = sroot;
      etiq = make_etiq() }
    (Mappe.map
       (fun n -> { 
	 succ = n.succ; 
	 etiq = make_etiq() })
       g.nodes)

let topological_sort_aux root nodes =
  let rec visit v res = 
    let nv = Mappe.find v nodes in
    if !(nv.etiq) then 
      res
    else
      begin
	nv.etiq := true;
	v :: (Sette.fold visit nv.succ res)
      end
  in
  visit root [] 

let topological_sort g root = 
  let nodes = squelette (fun () -> ref false) g in
  topological_sort_aux root nodes

let topological_sort_multi root g sroot = 
  let nodes = squelette_multi root  (fun () -> ref false) g sroot in
  List.tl (topological_sort_aux root nodes)


(* retourne les sommets inaccessibles a partir d'un sommet *)
let accessible_aux root nodes g =
  let rec visit v =
    let nv = Mappe.find v nodes in
    if not !(nv.etiq) then
      begin
	nv.etiq := true;
	Sette.iter visit nv.succ
      end
  in
  visit root ;
  Mappe.fold
    (begin fun v nv removed ->
      if !((Mappe.find v nodes).etiq) then
	removed
      else
	Sette.add v removed;
    end)
    g.nodes
    Sette.empty

let accessible g root =
  let nodes = squelette (fun () -> ref false) g in
  accessible_aux root nodes g

let accessible_multi root g sroot = 
  let nodes = squelette_multi root (fun () -> ref false) g sroot in
  accessible_aux root nodes g 

(* composantes fortement connexes *)
let cfc_aux root nodes = 
  let num       = ref(-1) and
      partition = ref [] and
      pile      = Stack.create()
  in
  let rec visit sommet =
    Stack.push sommet pile;
    let node = (Mappe.find sommet nodes) in
    incr num;
    node.etiq := !num;
    let head = 
      Sette.fold
	(fun succ head ->
	  let dfn = !((Mappe.find succ nodes).etiq) in
	  let m = if dfn=min_int then (visit succ) else dfn in
	  min m head)
	node.succ
	!num
    in
    if head = !(node.etiq) then 
      begin
	let element = ref (Stack.pop pile) in
	let composante = ref [!element] in
	  (Mappe.find !element nodes).etiq := max_int;
	  while !element <> sommet do
	    element := Stack.pop pile;
	    (Mappe.find !element nodes).etiq := max_int;
	    composante := !element :: !composante
	  done;
	partition := !composante :: !partition
      end;
    head
  in
  let _ = visit root in
  !partition

let cfc g root = 
  let nodes = squelette (fun () -> ref min_int) g in
  cfc_aux root nodes 

let cfc_multi root g sroot = 
  let nodes = squelette_multi root (fun () -> ref min_int) g sroot in
  match cfc_aux root nodes with
  | (x :: r) :: l when x=root -> r :: l
  | _ -> failwith "graph.ml: cfc_multi"

let scfc_aux root nodes = 
  let num       = ref(-1) and
      pile      = Stack.create()
  in
  let rec composante sommet =
    let partition = ref Nil in
    Sette.iter
      (function x -> 
	if !((Mappe.find x nodes).etiq) = min_int then begin
	  (visit x partition) ; ()
	end)
      (Mappe.find sommet nodes).succ;
    Cons(Atome sommet, !partition)
	
  and visit sommet partition =
    Stack.push sommet pile;
    incr num;
    let node = (Mappe.find sommet nodes) in
    node.etiq := !num;
    let head = ref !num and loop = ref false in
    (Sette.iter
       (fun succ ->
	 let dfn = !((Mappe.find succ nodes).etiq) in
	 let m =
	   if dfn=min_int 
	   then (visit succ partition)
	   else dfn 
	 in
	 if m <= !head then begin loop := true; head := m end)
       node.succ);
    if !head = !(node.etiq) then 
      begin
	node.etiq := max_int;
	let element = ref (Stack.pop pile) in
	if !loop then 
	  begin
	    while !element <> sommet do
	      (Mappe.find !element nodes).etiq := min_int;
	      element := Stack.pop pile
	    done;
	    partition := Cons(List(composante sommet),!partition )
	  end 
	else
	  partition := Cons(Atome(sommet),!partition)
      end;
    !head
  in
  let partition = ref Nil in
  let _ = visit root partition in
  !partition
    
let scfc g root = 
  let nodes = squelette (fun () -> ref min_int) g in
  scfc_aux root nodes

let scfc_multi root g sroot = 
  let nodes = squelette_multi root (fun () -> ref min_int) g sroot in
  let res = scfc_aux root nodes in
  match res with
  | Cons(Atome(x),l) when x=root -> l
  | _ -> failwith "graph.ml: scfc_multi"
	
let min g =
  let marks = squelette (fun () -> (ref true)) g in
  Mappe.iter
    (begin fun v nv ->
      Sette.iter
	(fun succ -> let flag = (Mappe.find succ marks).etiq in flag := false)
	nv.succ
    end)
    marks;
  Mappe.fold
    (fun v nv res -> if !(nv.etiq) then Sette.add v res else res)
    marks
    Sette.empty
let max g =
  Mappe.fold
    (fun sommet node res -> 
       if Sette.is_empty node.succ then Sette.add sommet res else res)
    g.nodes
    Sette.empty

let print print_vertex print_etiq print_arete formatter g = 
  let print v nv =
    fprintf formatter "v = %a, @[<hv>etiq = %a,@ succ = %a@]"
      print_vertex v
      print_etiq nv.etiq
      (Sette.print print_vertex) nv.succ
  in
  fprintf formatter "[ @[<hv>";
  (Mappe.iter 
     (fun v nv -> print v nv; fprintf formatter ",@ ")
     g.nodes);
  fprintf formatter "@]]@ ";
  fprintf formatter "[ @[<hv>";
  (Mappe.iter
     (fun (a,b) arete ->
       fprintf formatter "@[{(%a,%a),%a};@]@ "
	 print_vertex a
	 print_vertex b
	 print_arete arete)
     g.arcs);
  fprintf formatter "@]]";
  ()
    
