
open Format
open Ilist

(*  ********************************************************************** *)
(** {2 Polymorphic version} *)
(*  ********************************************************************** *)

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
	  Pervasives.min m head)
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

let scfc_aux root nodes = 
  let num       = ref(-1) and
      pile      = Stack.create()
  in
  let rec composante sommet =
    let partition = ref Nil in
    Sette.iter
      (function x -> 
	if !((Mappe.find x nodes).etiq) = min_int then begin
	  ignore (visit x partition)
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
    
(*  ********************************************************************** *)
(** {2 Functor version} *)
(*  ********************************************************************** *)

module type T = sig
  module MapV : Mappe.S
    (** Map module for associating attributes to vertices, of type [MapV.key]
      *)
  module MapE : (Mappe.S with type key = MapV.key * MapV.key)
    (** Map module for associating attributes to edges, of type [MapV.key *
      MapV.key] *)
end

module type S = sig
  type vertex
    (** The type of vertices *)
  module SetV : (Sette.S with type elt=vertex)
    (** The type of sets of vertices *)
  module SetE : (Sette.S with type elt=vertex*vertex)
    (** The type of sets of edges *)
  module MapV : (Mappe.S with type key=vertex and module Setkey=SetV)
    (** The Map for vertices *)
  module MapE : (Mappe.S with type key=vertex*vertex and module Setkey=SetE)
    (** The Map for edges *)
	    
  type ('b,'c) t
    (** The type of graphs, where:
      - ['b] is the type of vertex attribute (etiq);
      - ['c] is the type of edge attributes (arete)
    *)
    
  val succ : ('b,'c) t -> vertex -> SetV.t
  val pred : ('b,'c) t -> vertex -> SetV.t
  val etiq : ('b,'c) t -> vertex -> 'b
  val arete : ('b,'c) t -> vertex * vertex -> 'c
  val empty : ('b,'c) t
  val size : ('b,'c) t -> int
  val is_empty : ('b,'c) t -> bool
  val is_vertex : ('b,'c) t -> vertex -> bool
  val is_edge : ('b,'c) t -> vertex * vertex -> bool
  val vertices : ('b,'c) t -> SetV.t
  val edges : ('b,'c) t -> SetE.t

  val map_vertex : ('b,'c) t -> (vertex -> 'b -> 'd) -> ('d, 'c) t
  val map_edge : ('b,'c) t -> (vertex * vertex -> 'c -> 'd) -> ('b, 'd) t
  val iter_vertex : ('b,'c) t -> (vertex -> 'b -> SetV.t -> unit) -> unit
  val iter_edge : ('b,'c) t -> ((vertex * vertex) -> 'c -> unit) -> unit
  val fold_vertex : ('b,'c) t -> 'd -> (vertex -> 'b -> SetV.t -> 'd -> 'd) -> 'd
  val fold_edge : ('b,'c) t -> 'd -> (vertex * vertex -> 'c -> 'd -> 'd) -> 'd

  val add_edge : ('b,'c) t -> vertex * vertex -> 'c -> ('b,'c) t
  val remove_edge : ('b,'c) t -> vertex * vertex -> ('b,'c) t
  val add_vertex : ('b,'c) t -> vertex -> 'b -> ('b,'c) t
  val remove_vertex : ('b,'c) t -> vertex -> ('b,'c) t
  val topological_sort : ('b,'c) t -> vertex -> vertex list
  val topological_sort_multi : vertex -> ('b,'c) t -> SetV.t -> vertex list
  val accessible : ('b,'c) t -> vertex -> SetV.t
  val accessible_multi :
  vertex -> ('b,'c) t -> SetV.t -> SetV.t
  val cfc : ('b,'c) t -> vertex -> vertex list list
  val cfc_multi : vertex -> ('b,'c) t -> SetV.t -> vertex list list
  val scfc : ('b,'c) t -> vertex -> vertex Ilist.t
  val scfc_multi : vertex -> ('b,'c) t -> SetV.t -> vertex Ilist.t
  val min : ('b,'c) t -> SetV.t
  val max : ('b,'c) t -> SetV.t
  val print : (Format.formatter -> vertex -> unit) -> (Format.formatter -> 'b -> unit) -> (Format.formatter -> 'c -> unit) -> Format.formatter -> ('b,'c) t -> unit
end

module Make(T : T) : (S with type vertex=T.MapV.key
			and module SetV=T.MapV.Setkey
			and module SetE=T.MapE.Setkey
			and module MapV=T.MapV
			and module MapE=T.MapE)
  = 
struct

  type vertex = T.MapV.key
  module SetV = T.MapV.Setkey
  module SetE = T.MapE.Setkey
  module MapV = T.MapV
  module MapE = T.MapE

  type 'b node = {
    succ: SetV.t;
    etiq: 'b
  }
  type ('b,'c) t = {
    nodes: 'b node MapV.t;
    arcs: 'c MapE.t
  }
  
  let node g n = MapV.find n g.nodes
  let succ g n = (node g n).succ
  let etiq g n = (node g n).etiq
  
  let arete g arc = MapE.find arc g.arcs
  let empty = { nodes = MapV.empty; arcs = MapE.empty }
  let size g = MapV.fold (fun _ _ n -> n+1) g.nodes 0
  
  let is_empty g = (g.nodes = MapV.empty)
  let is_vertex g i =
    try let _ = node g i in true
    with Not_found -> false
  let is_edge g arc = 
    try let _ = MapE.find arc g.arcs in true
    with Not_found -> false
  
  let vertices g = MapV.fold (fun v nv set -> SetV.add v set) g.nodes SetV.empty
  let edges g = MapE.fold (fun arc arete set -> SetE.add arc set) g.arcs SetE.empty
  
  let map_vertex g f = {
    nodes = 
    MapV.mapi
      (fun v nv -> {
        succ = nv.succ;
        etiq = f v nv.etiq
      })
      g.nodes;
    arcs = g.arcs
  } 
  let iter_vertex g f = MapV.iter (fun v nv -> f v nv.etiq nv.succ) g.nodes
  let fold_vertex g r f = MapV.fold (fun v nv r -> f v nv.etiq nv.succ r) g.nodes r
  let map_edge g f = {
    nodes = g.nodes;
    arcs = MapE.mapi f g.arcs
  } 
  let iter_edge g f = MapE.iter f g.arcs
  let fold_edge g r f = MapE.fold f g.arcs r
  
  let pred g n = 
    fold_vertex g SetV.empty
      (fun v _ succ set -> if SetV.mem n succ then SetV.add v set else set)
  
  let add_edge g ((a,b) as arc) arete = 
    try {
      nodes = begin
        if is_edge g arc then
  	g.nodes
        else
  	let na = node g a in
  	MapV.add a 
  	  { succ = SetV.add b na.succ; etiq = na.etiq }
  	  g.nodes
      end;
      arcs = MapE.add arc arete g.arcs
    } 
    with Not_found -> failwith "SGraph.add_edge"
  
  let remove_edge g ((a,b) as arc) = 
    try {
      nodes = begin
        let na = node g a in
        MapV.add a 
  	{ succ = SetV.remove b na.succ; etiq = na.etiq }
  	g.nodes
      end;
      arcs = MapE.remove arc g.arcs
    } 
    with Not_found -> failwith "SGraph.remove_edge"
        
  let add_vertex g v etiq = 
    try
      let anode = MapV.find v g.nodes in 
      { nodes = MapV.add v { succ = anode.succ; etiq = etiq } g.nodes;
        arcs = g.arcs } 
    with Not_found -> 
      { nodes = MapV.add v { succ = SetV.empty; etiq = etiq } g.nodes;
        arcs = g.arcs } 
  
  let remove_vertex g v = 
    try
      let nv = node g v in 
      let g = 
        { nodes = MapV.remove v g.nodes;
  	arcs = 
  	SetV.fold
  	  (fun succ arcs -> MapE.remove (v,succ) arcs)
  	  nv.succ
  	  g.arcs }
      in
      SetV.fold
        (fun p g -> remove_edge g (p,v))
        (pred g v)
        g
    with Not_found ->
      raise (Failure "SGraph.remove_vertex")
  
  (* fonctions auxiliaires *)
  let squelette make_etiq g =
    MapV.map 
      (fun n -> { succ = n.succ; etiq = make_etiq() })
      g.nodes
  
  let squelette_multi root make_etiq g sroot =
    MapV.add 
      root
      { succ = sroot;
        etiq = make_etiq() }
      (MapV.map
         (fun n -> { 
  	 succ = n.succ; 
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
    | [x] :: r when (SetV.Ord.compare x root)=0 -> r 
    | _ -> failwith "graph.ml: cfc_multi"
  
  let scfc_aux root nodes = 
    let num       = ref(-1) and
        pile      = Stack.create()
    in
    let rec composante sommet =
      let partition = ref Nil in
      SetV.iter
        (function x -> 
  	if !((MapV.find x nodes).etiq) = min_int then begin
  	  ignore (visit x partition)
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
    | Cons(Atome(x),l) when (SetV.Ord.compare x root)=0 -> l
    | _ -> failwith "graph.ml: scfc_multi"
  	
  let min g =
    let marks = squelette (fun () -> (ref true)) g in
    MapV.iter
      (begin fun v nv ->
        SetV.iter
  	(fun succ -> let flag = (MapV.find succ marks).etiq in flag := false)
  	nv.succ
      end)
      marks;
    MapV.fold
      (fun v nv res -> if !(nv.etiq) then SetV.add v res else res)
      marks
      SetV.empty
  let max g =
    MapV.fold
      (fun sommet node res -> 
         if SetV.is_empty node.succ then SetV.add sommet res else res)
      g.nodes
      SetV.empty
  
  let print print_vertex print_etiq print_arete formatter g = 
    let print v nv =
      fprintf formatter "v = %a, @[<hv>etiq = %a,@ succ = %a@]"
        print_vertex v
        print_etiq nv.etiq
        (SetV.print print_vertex) nv.succ
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
  
end
  
    
  
