(* % $Id: graph.ml,v 1.6 2005/06/14 14:19:56 bjeannet Exp $ *)

(** Operations on directed graphs. (Two-way information maintained) *)




open Format
open Ilist

(* ************************************************************************ *)
(* version fonctionnelle *)
(* ************************************************************************ *)

(* ======================================================================== *)
(* Datatypes *)
(* ======================================================================== *)

type ('a,'b) node = {
    succ: 'a Sette.t;
    pred: 'a Sette.t;
    etiq: 'b
  }
type ('a,'b,'c) t = {
    nodes: ('a, ('a,'b) node) Mappe.t;
    arcs: ('a*'a,'c) Mappe.t
  }

let node g n = Mappe.find n g.nodes
let succ g n = (node g n).succ
let pred g n = (node g n ).pred
let etiq g n = (node g n).etiq
let arete g arc = Mappe.find arc g.arcs
let empty = { nodes = Mappe.empty; arcs = Mappe.empty }
let size g =
  (Mappe.fold (fun _ _ n -> n+1) g.nodes 0,
   Mappe.fold (fun _ _ n -> n+1) g.arcs 0)

let is_empty g = (g.nodes = Mappe.empty)
let is_vertex g i =
  try let _ = node g i in true
  with Not_found -> false
let is_edge g arc =
  try let _ = Mappe.find arc g.arcs in true
  with Not_found -> false

let vertices g = Mappe.maptoset g.nodes
let edges g = Mappe.maptoset g.arcs

let add_edge g ((a,b) as arc) arete =
  try {
    nodes = begin
      if is_edge g arc then
	g.nodes
      else
	let na = node g a in
	if a=b then
	  Mappe.add a
	    { succ = Sette.add a na.succ; pred = Sette.add b na.pred; etiq = na.etiq }
	    g.nodes
	else
	  let nb = node g b in
	  Mappe.add a
	    { succ = Sette.add b na.succ; pred = na.pred; etiq = na.etiq }
	    (Mappe.add b
	       { succ = nb.succ; pred = Sette.add a nb.pred; etiq = nb.etiq }
	       g.nodes)
    end;
    arcs = Mappe.add arc arete g.arcs
  }
  with Not_found -> failwith "graph.ml: add_edge"

let remove_edge g ((a,b) as arc) =
  try {
    nodes = begin
      let na = node g a in
      if a=b then
	Mappe.add a
	  { succ = Sette.remove a na.succ; pred = Sette.remove a na.pred; etiq = na.etiq }
	  g.nodes
      else
	let nb = node g b in
	Mappe.add a
	  { succ = Sette.remove b na.succ; pred = na.pred; etiq = na.etiq }
	    (Mappe.add b
	       { succ = nb.succ; pred = Sette.remove a nb.pred; etiq = nb.etiq }
	       g.nodes)
    end;
    arcs = Mappe.remove arc g.arcs
  }
  with Not_found -> failwith "graph.ml: remove_edge"

let add_vertex g v etiq =
  try
    let anode = Mappe.find v g.nodes in {
    nodes = Mappe.add v { succ = anode.succ; pred = anode.pred; etiq = etiq }
      g.nodes;
    arcs = g.arcs
  }
  with Not_found -> {
    nodes = Mappe.add v { succ = Sette.empty; pred = Sette.empty; etiq = etiq } g.nodes;
    arcs = g.arcs
  }

let remove_vertex g a =
  try
    let na = node g a in
    let g =
      Sette.fold
	(fun succ g -> remove_edge g (a,succ))
	na.succ
	(Sette.fold
	   (fun pred g -> remove_edge g (pred,a))
	   na.pred g)
    in
    { nodes = Mappe.remove a g.nodes; arcs = g.arcs }
  with Not_found ->
    raise (Failure "graphes.ml : remove_vertex")

let duplicate_vertex g v nv etiq =
  try
    let node = node g v in
    let gr = ref g in
    gr := add_vertex !gr nv etiq;
    let (su,pr) =
      if Sette.mem v node.succ then
	(Sette.remove v node.succ, Sette.remove v node.pred)
      else
	(node.succ, node.pred)
    in
    Sette.iter
      (fun su -> gr := add_edge !gr (nv,su) (Mappe.find (v,su) g.arcs))
      su;
    Sette.iter
      (fun pr -> gr := add_edge !gr (pr,nv) (Mappe.find (pr,v) g.arcs))
      pr;
    if Sette.mem v node.succ then
      gr := add_edge !gr (nv,nv) (Mappe.find (v,v) g.arcs);
    !gr
  with Not_found ->
    raise (Failure "graphes.ml : duplicate_vertex")

let transpose g = {
  nodes = Mappe.map (fun n -> { succ = n.pred; pred = n.succ; etiq = n.etiq }) g.nodes;
  arcs = Mappe.fold (fun (a,b) arete g -> Mappe.add (b,a) arete g) g.arcs Mappe.empty
}


(* ======================================================================== *)
(* fonctions auxiliaires *)
(* ======================================================================== *)

let squelette make_etiq g =
  Mappe.map
    (fun n -> { succ = n.succ; pred = n.pred; etiq = make_etiq() })
    g.nodes

let squelette_multi root make_etiq g sroot =
  Mappe.add
    root
    { succ = Sette.fold Sette.add sroot Sette.empty;
      pred = Sette.empty;
      etiq = make_etiq() }
    (Mappe.mapi
       (fun v n -> {
	 succ = n.succ;
	 pred = (if Sette.mem v sroot then Sette.add root n.pred else n.pred) ;
	 etiq = make_etiq() })
       g.nodes)

let squelette_filter_multi 
  (root:'a) 
  (make_etiq:unit -> 'd)
  (filter:'a -> 'a -> bool)
  (g:('a,'b,'c) t)
  (sroot:'a Sette.t) 
  : 
  ('a, ('a,'d) node) Mappe.t
  =
  Mappe.add
    root
    { succ = Sette.fold Sette.add sroot Sette.empty;
      pred = Sette.empty;
      etiq = make_etiq() }
    (Mappe.mapi
       (fun v n -> {
	 succ = begin
	   Sette.fold
	     (begin fun succ res ->
	       if not (filter v succ) then Sette.remove succ res else res
	     end)
	     n.succ
	     n.succ
	 end;
	 pred = begin
	   Sette.fold
	     (begin fun pred res ->
	       if not (filter pred v) then Sette.remove pred res else res
	     end)
	     n.pred
	     (if Sette.mem v sroot then Sette.add root n.pred else n.pred) ;
	 end;
	 etiq = make_etiq() })
      g.nodes)

let cosquelette_multi root make_etiq g sroot =
  Mappe.add
    root
    { succ = Sette.empty;
      pred = Sette.fold Sette.add sroot Sette.empty;
      etiq = make_etiq() }
    (Mappe.mapi
       (fun v n -> {
	 succ = (if Sette.mem v sroot then Sette.add root n.succ else n.succ);
	 pred =  n.pred;
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


(* ======================================================================== *)
(* retourne les sommets inaccessibles a partir d'un sommet *)
(* ======================================================================== *)
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

(* ======================================================================== *)
(* enleve les sommets ne pouvant mener a un sommet *)
(* ======================================================================== *)
let coaccessible_aux root nodes g =
  let rec visit v =
    let nv = Mappe.find v nodes in
    if not !(nv.etiq) then
      begin
	nv.etiq := true;
	Sette.iter visit nv.pred
      end
  in
  visit root ;
  Mappe.fold
    (begin fun v nv removed ->
      if !((Mappe.find v nodes).etiq) then
	removed
      else
	Sette.add v removed
    end)
    g.nodes
    Sette.empty

let coaccessible g root =
  let nodes = squelette (fun () -> ref false) g in
  coaccessible_aux root nodes g

let coaccessible_multi root g sroot =
  let nodes = cosquelette_multi root (fun () -> ref false) g sroot in
  coaccessible_aux root nodes g

(* ======================================================================== *)
(* composantes fortement connexes *)
(* ======================================================================== *)
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
  | [x] :: r when x=root -> r
  | _ -> failwith "graph.ml: cfc_multi"

let cfc_filter_multi root filter g sroot =
  let nodes = squelette_filter_multi root (fun () -> ref min_int) filter g sroot in
  match cfc_aux root nodes with
  | [x] :: r when x=root -> r
  | _ -> failwith "graph.ml: cfc_filter_multi"

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

let scfc_filter_multi root filter g sroot =
  let nodes = squelette_filter_multi root (fun () -> ref min_int) filter g sroot in
  let res = scfc_aux root nodes in
  match res with
  | Cons(Atome(x),l) when x=root -> l
  | _ -> failwith "graph.ml: scfc_filter_multi"

let min g =
  Mappe.fold
    (fun sommet node res ->
       if Sette.is_empty node.pred then Sette.add sommet res else res)
    g.nodes
    Sette.empty
let max g =
  Mappe.fold
    (fun sommet node res ->
       if Sette.is_empty node.succ then Sette.add sommet res else res)
    g.nodes
    Sette.empty

let print print_vertex print_etiq print_arete formatter g =
  let print v nv =
    fprintf formatter "v = %a, @[<hv>etiq = %a,@ succ = %a,@ pred = %a@]"
      print_vertex v
      print_etiq nv.etiq
      (Sette.print print_vertex) nv.succ
      (Sette.print print_vertex) nv.pred
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

let print_dot
  ?(titlestyle:string="shape=ellipse,style=bold,style=filled,fontsize=20")
  ?(vertexstyle:string="shape=box,fontsize=12")
  ?(edgestyle:string="fontsize=12")
  ?(title:string="")
  print_vertex print_etiq print_arete
  fmt g
  =
  fprintf fmt "digraph G {@.  @[<v>";
  if title<>"" then
    fprintf fmt "1073741823 [%s,label=\"%s\"];@ " titlestyle title;
  Mappe.iter
    (begin fun v nv ->
      fprintf fmt "%a [%s,label=\"%t\"];@ "
      print_vertex v
      vertexstyle
      (fun fmt -> print_etiq fmt v nv.etiq);
      Sette.iter
	(begin fun succ ->
	  let edge = (v,succ) in
	  let arete = Mappe.find edge g.arcs in
	  fprintf fmt "%a -> %a [%s,label=\"%t\"];@ "
	    print_vertex v print_vertex succ
	    edgestyle
	    (fun fmt -> print_arete fmt edge arete);
	end)
	nv.succ;
    end)
    g.nodes;
  fprintf fmt "@]@.}@.";
  ()

let map_vertex g f = {
  nodes =
  Mappe.mapi
    (fun v nv -> {
      succ = nv.succ;
      pred = nv.pred;
      etiq = f v nv.etiq
    })
    g.nodes;
  arcs = g.arcs
}
let iter_vertex g f = Mappe.iter (fun v nv -> f v nv.etiq ) g.nodes
let fold_vertex g r f = Mappe.fold (fun v nv r -> f v nv.etiq r) g.nodes r
let map_edge g f = {
  nodes = g.nodes;
  arcs = Mappe.mapi f g.arcs
}
let iter_edge g f = Mappe.iter f g.arcs
let fold_edge g r f = Mappe.fold f g.arcs r

let map g fb fc = {
  nodes =
  Mappe.mapi
    (fun v nv -> {
      succ = nv.succ;
      pred = nv.pred;
      etiq = fb v nv.etiq
    })
    g.nodes;
  arcs = Mappe.mapi fc g.arcs
}

(* ************************************************************************ *)
(* version foncteur *)
(* ************************************************************************ *)

module type Param = sig
  include Graph.Param
end

module type S = sig
  include Graph.S
end

module Make(P : Param) = struct
  type vertex = P.MapV.key
  module SetV = P.MapV.Setkey
  module MapV = P.MapV
  module MapE = P.MapE

  type 'b node = {
    succ: SetV.t;
    pred: SetV.t;
    etiq: 'b
  }
  type ('b,'c) t = {
    nodes: 'b node MapV.t;
    arcs: 'c MapE.t
  }

  let node g n = MapV.find n g.nodes
  let succ g n = (node g n).succ
  let pred g n = (node g n ).pred
  let etiq g n = (node g n).etiq
  let arete g arc = MapE.find arc g.arcs
  let empty = { nodes = MapV.empty; arcs = MapE.empty }
  let size g =
    (MapV.fold (fun _ _ n -> n+1) g.nodes 0,
    MapE.fold (fun _ _ n -> n+1) g.arcs 0)

  let is_empty g = (g.nodes = MapV.empty)
  let is_vertex g i =
    try let _ = node g i in true
    with Not_found -> false
  let is_edge g arc =
    try let _ = MapE.find arc g.arcs in true
    with Not_found -> false

  let vertices g = MapV.maptoset g.nodes
  let edges g = MapE.maptoset g.arcs

  let add_edge g ((a,b) as arc) arete =
    try {
      nodes = begin
	if is_edge g arc then
	  g.nodes
	else
	  let na = node g a in
	  if a=b then
	    MapV.add a
	      { succ = SetV.add a na.succ; pred = SetV.add b na.pred; etiq = na.etiq }
	      g.nodes
	  else
	    let nb = node g b in
	    MapV.add a
	      { succ = SetV.add b na.succ; pred = na.pred; etiq = na.etiq }
	      (MapV.add b
		{ succ = nb.succ; pred = SetV.add a nb.pred; etiq = nb.etiq }
		g.nodes)
      end;
      arcs = MapE.add arc arete g.arcs
    }
    with Not_found -> failwith "graph.ml: add_edge"

  let remove_edge g ((a,b) as arc) =
    try {
      nodes = begin
	let na = node g a in
	if a=b then
	  MapV.add a
	    { succ = SetV.remove a na.succ; pred = SetV.remove a na.pred; etiq = na.etiq }
	    g.nodes
	else
	  let nb = node g b in
	  MapV.add a
	    { succ = SetV.remove b na.succ; pred = na.pred; etiq = na.etiq }
	    (MapV.add b
	      { succ = nb.succ; pred = SetV.remove a nb.pred; etiq = nb.etiq }
	      g.nodes)
      end;
      arcs = MapE.remove arc g.arcs
    }
    with Not_found -> failwith "graph.ml: remove_edge"

  let add_vertex g v etiq =
    try
      let anode = MapV.find v g.nodes in {
	nodes = MapV.add v { succ = anode.succ; pred = anode.pred; etiq = etiq }
	g.nodes;
	arcs = g.arcs
      }
    with Not_found -> {
      nodes = MapV.add v { succ = SetV.empty; pred = SetV.empty; etiq = etiq } g.nodes;
      arcs = g.arcs
    }

  let remove_vertex g a =
    try
      let na = node g a in
      let g =
	SetV.fold
	  (fun succ g -> remove_edge g (a,succ))
	  na.succ
	  (SetV.fold
	    (fun pred g -> remove_edge g (pred,a))
	    na.pred g)
      in
      { nodes = MapV.remove a g.nodes; arcs = g.arcs }
    with Not_found ->
      raise (Failure "graphes.ml : remove_vertex")

  let duplicate_vertex g v nv etiq =
    try
      let node = node g v in
      let gr = ref g in
      gr := add_vertex !gr nv etiq;
      let (su,pr) =
	if SetV.mem v node.succ then
	  (SetV.remove v node.succ, SetV.remove v node.pred)
	else
	  (node.succ, node.pred)
      in
      SetV.iter
	(fun su -> gr := add_edge !gr (nv,su) (MapE.find (v,su) g.arcs))
	su;
      SetV.iter
	(fun pr -> gr := add_edge !gr (pr,nv) (MapE.find (pr,v) g.arcs))
	pr;
      if SetV.mem v node.succ then
	gr := add_edge !gr (nv,nv) (MapE.find (v,v) g.arcs);
      !gr
    with Not_found ->
      raise (Failure "graphes.ml : duplicate_vertex")

  let transpose g = {
    nodes = MapV.map (fun n -> { succ = n.pred; pred = n.succ; etiq = n.etiq }) g.nodes;
    arcs = MapE.fold (fun (a,b) arete g -> MapE.add (b,a) arete g) g.arcs MapE.empty
  }

  (* fonctions auxiliaires *)
  let squelette make_etiq g =
    MapV.map
      (fun n -> { succ = n.succ; pred = n.pred; etiq = make_etiq() })
      g.nodes

  let squelette_multi root make_etiq g sroot =
    MapV.add
      root
      { succ = SetV.fold SetV.add sroot SetV.empty;
      pred = SetV.empty;
      etiq = make_etiq() }
      (MapV.mapi
	(fun v n -> {
	  succ = n.succ;
	  pred = (if SetV.mem v sroot then SetV.add root n.pred else n.pred) ;
	  etiq = make_etiq() })
	g.nodes)

  let squelette_filter_multi 
    (root:vertex) 
    (make_etiq:unit -> 'd)
    (filter:vertex -> vertex -> bool)
    (g:('b,'c) t)
    (sroot:SetV.t) 
    : 
    ('d node) MapV.t
    =
    MapV.add
      root
      { succ = SetV.fold SetV.add sroot SetV.empty;
      pred = SetV.empty;
      etiq = make_etiq() }
      (MapV.mapi
	(fun v n -> {
	  succ = begin
	    SetV.fold
	      (begin fun succ res ->
		if not (filter v succ) then SetV.remove succ res else res
	      end)
	      n.succ
	      n.succ
	  end;
	  pred = begin
	    SetV.fold
	      (begin fun pred res ->
		if not (filter pred v) then SetV.remove pred res else res
	      end)
	      n.pred
	      (if SetV.mem v sroot then SetV.add root n.pred else n.pred) ;
	  end;
	  etiq = make_etiq() })
	g.nodes)

  let cosquelette_multi root make_etiq g sroot =
    MapV.add
      root
      { succ = SetV.empty;
      pred = SetV.fold SetV.add sroot SetV.empty;
      etiq = make_etiq() }
      (MapV.mapi
	(fun v n -> {
	  succ = (if SetV.mem v sroot then SetV.add root n.succ else n.succ);
	  pred =  n.pred;
	  etiq = make_etiq() })
	g.nodes)

  let topological_sort_aux root nodes =
    let rec visit v res =
      let nv = MapV.find v nodes in
      if !(nv.etiq) then
	res
      else
	begin
	  nv.etiq := true;
	  v :: (SetV.fold visit nv.succ res)
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
      let nv = MapV.find v nodes in
      if not !(nv.etiq) then
	begin
	  nv.etiq := true;
	  SetV.iter visit nv.succ
	end
    in
    visit root ;
    MapV.fold
      (begin fun v nv removed ->
	if !((MapV.find v nodes).etiq) then
	  removed
	else
	  SetV.add v removed;
      end)
      g.nodes
      SetV.empty

  let accessible g root =
    let nodes = squelette (fun () -> ref false) g in
    accessible_aux root nodes g

  let accessible_multi root g sroot =
    let nodes = squelette_multi root (fun () -> ref false) g sroot in
    accessible_aux root nodes g

  (* enleve les sommets ne pouvant mener a un sommet *)
  let coaccessible_aux root nodes g =
    let rec visit v =
      let nv = MapV.find v nodes in
      if not !(nv.etiq) then
	begin
	  nv.etiq := true;
	  SetV.iter visit nv.pred
	end
    in
    visit root ;
    MapV.fold
      (begin fun v nv removed ->
	if !((MapV.find v nodes).etiq) then
	  removed
	else
	  SetV.add v removed
      end)
      g.nodes
      SetV.empty

  let coaccessible g root =
    let nodes = squelette (fun () -> ref false) g in
    coaccessible_aux root nodes g

  let coaccessible_multi root g sroot =
    let nodes = cosquelette_multi root (fun () -> ref false) g sroot in
    coaccessible_aux root nodes g

  (* composantes fortement connexes *)
  let cfc_aux root nodes =
    let num       = ref(-1) and
      partition = ref [] and
      pile      = Stack.create()
    in
    let rec visit sommet =
      Stack.push sommet pile;
      let node = (MapV.find sommet nodes) in
      incr num;
      node.etiq := !num;
      let head =
	SetV.fold
	  (fun succ head ->
	    let dfn = !((MapV.find succ nodes).etiq) in
	    let m = if dfn=min_int then (visit succ) else dfn in
	    Pervasives.min m head)
	  node.succ
	  !num
      in
      if head = !(node.etiq) then
	begin
	  let element = ref (Stack.pop pile) in
	  let composante = ref [!element] in
	  (MapV.find !element nodes).etiq := max_int;
	  while !element <> sommet do
	    element := Stack.pop pile;
	    (MapV.find !element nodes).etiq := max_int;
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

  let cfc_filter_multi root filter g sroot =
    let nodes = squelette_filter_multi root (fun () -> ref min_int) filter g sroot in
    match cfc_aux root nodes with
    | (x :: r) :: l when x=root -> r :: l
    | _ -> failwith "graph.ml: cfc_filter_multi"

  let scfc_aux root nodes =
    let num       = ref(-1) and
      pile      = Stack.create()
    in
    let rec composante sommet =
      let partition = ref Nil in
      SetV.iter
	(function x ->
	  if !((MapV.find x nodes).etiq) = min_int then begin
	    (visit x partition) ; ()
	  end)
	(MapV.find sommet nodes).succ;
      Cons(Atome sommet, !partition)

    and visit sommet partition =
      Stack.push sommet pile;
      incr num;
      let node = (MapV.find sommet nodes) in
      node.etiq := !num;
      let head = ref !num and loop = ref false in
      (SetV.iter
	(fun succ ->
	  let dfn = !((MapV.find succ nodes).etiq) in
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
		(MapV.find !element nodes).etiq := min_int;
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

  let scfc_filter_multi root filter g sroot =
    let nodes = squelette_filter_multi root (fun () -> ref min_int) filter g sroot in
    let res = scfc_aux root nodes in
    match res with
    | Cons(Atome(x),l) when x=root -> l
    | _ -> failwith "graph.ml: scfc_filter_multi"

  let min g =
    MapV.fold
      (fun sommet node res ->
	if SetV.is_empty node.pred then SetV.add sommet res else res)
      g.nodes
      SetV.empty
  let max g =
    MapV.fold
      (fun sommet node res ->
	if SetV.is_empty node.succ then SetV.add sommet res else res)
      g.nodes
      SetV.empty

  let print print_vertex print_etiq print_arete formatter g =
    let print v nv =
      fprintf formatter "v = %a, @[<hv>etiq = %a,@ succ = %a,@ pred = %a@]"
	print_vertex v
	print_etiq nv.etiq
	(SetV.print print_vertex) nv.succ
	(SetV.print print_vertex) nv.pred
    in
    fprintf formatter "[ @[<hv>";
    (MapV.iter
      (fun v nv -> print v nv; fprintf formatter ",@ ")
      g.nodes);
    fprintf formatter "@]]@ ";
    fprintf formatter "[ @[<hv>";
    (MapE.iter
      (fun (a,b) arete ->
	fprintf formatter "@[{(%a,%a),%a};@]@ "
	print_vertex a
	print_vertex b
	print_arete arete)
      g.arcs);
    fprintf formatter "@]]";
    ()

let print_dot
  ?(titlestyle:string="shape=ellipse,style=bold,style=filled,fontsize=20")
  ?(vertexstyle:string="shape=box,fontsize=12")
  ?(edgestyle:string="fontsize=12")
  ?(title:string="")
  print_vertex print_etiq print_arete
  fmt g
  =
  fprintf fmt "digraph G {@.  @[<v>";
  if title<>"" then
    fprintf fmt "1073741823 [%s,label=\"%s\"];@ " titlestyle title;
  MapV.iter
    (begin fun v nv ->
      fprintf fmt "%a [%s,label=\"%t\"];@ "
      print_vertex v
      vertexstyle
      (fun fmt -> print_etiq fmt v nv.etiq);
      SetV.iter
	(begin fun succ ->
	    let edge = (v,succ) in
	    let arete = MapE.find edge g.arcs in
	    fprintf fmt "%a -> %a [%s,label=\"%t\"];@ "
	      print_vertex v print_vertex succ
	      edgestyle
	      (fun fmt -> print_arete fmt edge arete);
	  end)
	 nv.succ;
     end)
    g.nodes;
  fprintf fmt "@]@.}@.";
  ()

  let map_vertex g f = {
    nodes =
    MapV.mapi
      (fun v nv -> {
	succ = nv.succ;
	pred = nv.pred;
	etiq = f v nv.etiq
      })
      g.nodes;
    arcs = g.arcs
  }
  let iter_vertex g f = MapV.iter (fun v nv -> f v nv.etiq ) g.nodes
  let fold_vertex g r f = MapV.fold (fun v nv r -> f v nv.etiq r) g.nodes r
  let map_edge g f = {
    nodes = g.nodes;
    arcs = MapE.mapi f g.arcs
  }
  let iter_edge g f = MapE.iter f g.arcs
  let fold_edge g r f = MapE.fold f g.arcs r

  let map g fb fc = {
    nodes =
    MapV.mapi
      (fun v nv -> {
	succ = nv.succ;
	pred = nv.pred;
	etiq = fb v nv.etiq
      })
      g.nodes;
    arcs = MapE.mapi fc g.arcs
  }
end
