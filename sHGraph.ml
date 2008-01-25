(* % $Id$ *)

(* % Bertrand Jeannet. This file is released under LGPL license. *)

(* \chapter{SHGraph: kind of hypergraphs} *)

(* This file is released under LGPL license. *)

(* We implement here oriented hypergraphs, the edges of which has a
   single origin vertex and are called nails. The implementation is
   done by side-effect. *)

open Format

let array_forall f tab =
  let res = ref true in
  for i=0 to pred (Array.length tab) do
    res := !res && f tab.(i)
  done;
  !res

let array_exists f tab =
  let res = ref false in
  for i=0 to pred (Array.length tab) do
    res := !res || f tab.(i)
  done;
  !res

type ('b,'c) vertex_n = {
  attrvertex : 'c;
  mutable predhedge : 'b Sette.t;
  mutable succhedge : 'b Sette.t;
}
type ('a,'d) hedge_n = {
  attrhedge : 'd;
  predvertex : 'a array;
  succvertex : 'a array;
}
type ('a,'b, 'c, 'd, 'e) t = {
  vertex : ('a, ('b,'c) vertex_n) Hashhe.t;
  hedge : ('b, ('a,'d) hedge_n) Hashhe.t;
  info : 'e
}

let create size info = {
  vertex = Hashhe.create size;
  hedge = Hashhe.create size;
  info = info;
}

let clear g =
  Hashhe.clear g.vertex;
  Hashhe.clear g.hedge;
  ()

type 'a priority =
  | All
  | Filter of ('a -> bool)
  | Priority of ('a -> int)

let filter_priority (p:'a priority)
  :
  'a -> bool
  =
  begin match p with
  | All -> fun x -> true
  | Filter f -> fun x -> f x
  | Priority f -> fun x -> f x >= 0
  end

let compare_priority (p:'a priority)
  :
  'a -> 'a -> int
  =
   begin match p with
  | All | Filter _ -> fun x y -> 0
  | Priority f ->
      fun x y -> let x = f x and y = f y in Pervasives.compare y x
   end


(* *********************************************************************** *)
(* Internal functions *)
(* *********************************************************************** *)
let vertex_n g v = Hashhe.find g.vertex v
let hedge_n g n = Hashhe.find g.hedge n

(* *********************************************************************** *)
(* Information *)
(* *********************************************************************** *)
let size_vertex g =
  Hashhe.length g.vertex
let size_hedge g =
  Hashhe.length g.hedge
let size_edgevh g =
  let n = ref 0 in
  Hashhe.iter
    (fun _ vertex_n ->
      n := !n + Sette.cardinal vertex_n.succhedge)
    g.vertex;
  !n
let size_edgehv g =
  let n = ref 0 in
  Hashhe.iter
    (fun _ hedge_n ->
      n := !n + Array.length hedge_n.succvertex)
    g.hedge;
  !n

let size g =
  (size_vertex g, size_hedge g, size_edgevh g, size_edgehv g)

(* *********************************************************************** *)
(* Access functions *)
(* *********************************************************************** *)
let attrvertex g v = (vertex_n g v).attrvertex
let attrhedge g n = (hedge_n g n).attrhedge
let info g = g.info

(* *********************************************************************** *)
(* Test functions *)
(* *********************************************************************** *)
let is_vertex g v = Hashhe.mem g.vertex v
let is_hedge g n = Hashhe.mem g.hedge n
let is_empty g =
  try
    Hashhe.iter
	(fun _ _ -> raise Exit)
	g.vertex;
    true;
  with Exit ->
    false

(* *********************************************************************** *)
(* Successors and predecessors *)
(* *********************************************************************** *)
let succhedge g v = (vertex_n g v).succhedge
let predhedge g v = (vertex_n g v).predhedge
let succvertex g n = (hedge_n g n).succvertex
let predvertex g n = (hedge_n g n).predvertex

(* *********************************************************************** *)
(* Addition of nodes and edges *)
(* *********************************************************************** *)
let add_vertex g v attrvertex =
  Hashhe.replace g.vertex
    v { attrvertex=attrvertex; succhedge=Sette.empty; predhedge=Sette.empty }

let add_hedge g h attrhedge ~pred ~succ =
  begin try
    Array.iter
      (fun v ->
	let vertex_n =  vertex_n g v in
	vertex_n.succhedge <- Sette.add h vertex_n.succhedge;
      )
      pred;
    Array.iter
      (fun v ->
	let vertex_n =  vertex_n g v in
	vertex_n.predhedge <- Sette.add h vertex_n.predhedge;
      )
      succ;
  with Not_found ->
    failwith "SHGraph.Make().add_hedge: origin vertex and/or destination hedge doesn't already exist"
  end;
  Hashhe.replace g.hedge
    h { attrhedge=attrhedge; predvertex=pred; succvertex=succ }

(* *********************************************************************** *)
(* Removal of nodes and edges *)
(* *********************************************************************** *)

let remove_hedge g h =
  try
    let hedge_n = hedge_n g h in
    Array.iter
      (begin fun v ->
	let vertex_n = vertex_n g v in
	vertex_n.succhedge <- Sette.remove h vertex_n.succhedge
      end)
      hedge_n.predvertex;
    Array.iter
      (begin fun v ->
	let vertex_n = vertex_n g v in
	vertex_n.predhedge <- Sette.remove h vertex_n.predhedge
      end)
      hedge_n.succvertex;
    Hashhe.remove g.hedge h;
  with Not_found ->
    ()

let remove_vertex g v =
  try
    let vertex_n = vertex_n g v in
    Sette.iter (fun h -> remove_hedge g h) vertex_n.predhedge;
    Sette.iter (fun h -> remove_hedge g h) vertex_n.succhedge;
    Hashhe.remove g.vertex v
  with Not_found ->
    ()

(* *********************************************************************** *)
(* Iterators *)
(* *********************************************************************** *)

let iter_vertex g f =
  Hashhe.iter
    (fun v vertex_n -> f v vertex_n.attrvertex ~pred:vertex_n.predhedge ~succ:vertex_n.succhedge)
    g.vertex
let iter_hedge g f =
  Hashhe.iter
    (fun n hedge_n -> f n hedge_n.attrhedge ~pred:hedge_n.predvertex ~succ:hedge_n.succvertex)
    g.hedge

let fold_vertex g f res =
  Hashhe.fold
    (fun v vertex_n res -> f v vertex_n.attrvertex ~pred:vertex_n.predhedge ~succ:vertex_n.succhedge  res)
    g.vertex res
let fold_hedge g f res =
  Hashhe.fold
    (fun n hedge_n res -> f n hedge_n.attrhedge ~pred:hedge_n.predvertex ~succ:hedge_n.succvertex res)
    g.hedge res

let map g map_attrvertex map_attrhedge map_info = {
  vertex = begin
    Hashhe.map
      (fun v vertex_n -> {
	attrvertex = map_attrvertex v vertex_n.attrvertex ~pred:vertex_n.predhedge ~succ:vertex_n.succhedge;
	predhedge = vertex_n.predhedge;
	succhedge = vertex_n.succhedge
      })
      g.vertex
  end;
  hedge = begin
    Hashhe.map
      (fun h hedge_n -> {
	attrhedge = map_attrhedge h hedge_n.attrhedge ~pred:hedge_n.predvertex ~succ:hedge_n.succvertex;
	predvertex = hedge_n.predvertex;
	succvertex = hedge_n.succvertex
      })
      g.hedge
  end;
  info = map_info g.info
}

(* *********************************************************************** *)
(* Misc functions *)
(* *********************************************************************** *)

let succ_vertex g v =
  let succhedge = succhedge g v in
  Sette.fold
    (begin fun h res ->
      let succvertex = succvertex g h in
      Array.fold_left
	(fun res v -> Sette.add v res)
	res succvertex
    end)
    succhedge Sette.empty

let pred_vertex g v =
  let predhedge = predhedge g v in
  Sette.fold
    (begin fun h res ->
      let predvertex = predvertex g h in
      Array.fold_left
	(fun res v -> Sette.add v res)
	res predvertex
    end)
    predhedge Sette.empty

(* *********************************************************************** *)
(* Copy *)
(* *********************************************************************** *)

let copy copy_attrvertex copy_attrhedge copy_info g = {
  vertex = begin
    Hashhe.map
      (fun v vertex_n -> {
	attrvertex = copy_attrvertex v vertex_n.attrvertex;
	predhedge = vertex_n.predhedge;
	succhedge = vertex_n.succhedge
      })
      g.vertex
  end;
  hedge = begin
    Hashhe.map
      (fun h hedge_n -> {
	attrhedge = copy_attrhedge h hedge_n.attrhedge;
	predvertex = hedge_n.predvertex;
	succvertex = hedge_n.succvertex
      })
      g.hedge
  end;
  info = copy_info g.info
}

let transpose copy_attrvertex copy_attrhedge copy_info g = {
  vertex = begin
    Hashhe.map
      (fun v vertex_n -> {
	attrvertex = copy_attrvertex v vertex_n.attrvertex;
	predhedge = vertex_n.succhedge;
	succhedge = vertex_n.predhedge
      })
      g.vertex
  end;
  hedge = begin
    Hashhe.map
      (fun h hedge_n -> {
	attrhedge = copy_attrhedge h hedge_n.attrhedge;
	predvertex = hedge_n.succvertex;
	succvertex = hedge_n.predvertex
      })
      g.hedge
  end;
  info = copy_info g.info
}

(* *********************************************************************** *)
(* Very internal functions *)
(* *********************************************************************** *)
let add_dummy_forward (vertex_dummy:'a) (hedge_dummy:'b) g (root:'a Sette.t) : unit =
  Hashhe.add g.vertex vertex_dummy
    { attrvertex=Obj.magic 0;
      succhedge=Sette.singleton hedge_dummy;
      predhedge=Sette.empty };
  Hashhe.add g.hedge hedge_dummy
    { attrhedge=Obj.magic 0;
      succvertex=begin
	let tab = Array.make (Sette.cardinal root) (Sette.choose root) in
	let i = ref 0 in
	Sette.iter (fun v -> tab.(!i) <- v; incr i) root;
	tab
      end;
      predvertex=[|vertex_dummy|] };
  ()

let rem_dummy (vertex_dummy:'a) (hedge_dummy:'b) g : unit =
  Hashhe.remove g.vertex vertex_dummy;
  Hashhe.remove g.hedge hedge_dummy

(* *********************************************************************** *)
(* Topological sort *)
(* *********************************************************************** *)

let topological_sort_aux
  ?(filter=(fun _ -> true))
  (g:('a,'b,'c,'d,'e) t)
  (root:'a)
  =
  let hash = Hashhe.create 23 in
  let rec visit res v =
    if Hashhe.mem hash v then
      res
    else begin
      Hashhe.add hash v ();
      Sette.fold
	(begin fun h res ->
	  if filter h then begin
	    let hedge_n = hedge_n g h in
	    let cond =
	      array_forall
		(Hashhe.mem hash)
		hedge_n.predvertex
	    in
	    if cond then begin
	      Array.fold_left
		visit
		res
		hedge_n.succvertex
	    end
	    else res
	  end
	  else
	    res
	end)
	(succhedge g v)
	(v::res)
    end
  in
  let res = visit [] root in
  List.rev res

let topological_sort g root =
  topological_sort_aux g root

let topological_sort_multi vertex_dummy hedge_dummy g root =
  add_dummy_forward vertex_dummy hedge_dummy g root;
  let res = topological_sort g vertex_dummy in
  rem_dummy vertex_dummy hedge_dummy g;
  List.tl res

let topological_sort_filter_multi vertex_dummy hedge_dummy g filter root =
  add_dummy_forward vertex_dummy hedge_dummy g root;
  let res =
    topological_sort_aux
      ~filter:(fun h -> if h = hedge_dummy then true else filter h)
      g vertex_dummy
  in
  rem_dummy vertex_dummy hedge_dummy g;
  List.tl res

(* *********************************************************************** *)
(* Reachability/Coreachability *)
(* *********************************************************************** *)
let reachable_aux
  ?(filter=(fun _ -> true))
  (g:('a,'b,'c,'d,'e) t)
  (root:'a)
  =
  let hashv = Hashhe.create 23 in
  let hashh = Hashhe.create 23 in
  let rec visit v =
    if not (Hashhe.mem hashv v) then begin
      Hashhe.add hashv v ();
      Sette.iter
	(begin fun h ->
	  if filter h && not (Hashhe.mem hashh h) then begin
	    let hedge_n = hedge_n g h in
	    let cond =
	      array_forall
		(Hashhe.mem hashv)
		hedge_n.predvertex
	    in
	    if cond then begin
	      Hashhe.add hashh h ();
	      Array.iter
		visit
		hedge_n.succvertex
	    end
	  end
	end)
	(succhedge g v)
    end
  in
  visit root;
  let setv =
    fold_vertex g
      (begin fun v _ ~pred:_ ~succ:_ res ->
	if not (Hashhe.mem hashv v)
	then Sette.add v res
	else res
      end)
      Sette.empty
  and setn =
    fold_hedge g
      (begin fun h _ ~pred:_ ~succ:_ res ->
	if not (Hashhe.mem hashh h)
	then Sette.add h res
	else res
      end)
      Sette.empty
  in
  (setv,setn)

let reachable g root =
  reachable_aux g root

let reachable_multi vertex_dummy hedge_dummy g root =
  add_dummy_forward vertex_dummy hedge_dummy g root;
  let res = reachable g vertex_dummy in
  rem_dummy vertex_dummy hedge_dummy g;
  let (vertices,hedges) = res in
  (Sette.remove vertex_dummy vertices,
  Sette.remove hedge_dummy hedges)

let reachable_filter_multi vertex_dummy hedge_dummy g filter root =
  add_dummy_forward vertex_dummy hedge_dummy g root;
  let res =
    reachable_aux
      ~filter:(fun h -> if h = hedge_dummy then true else filter h)
      g vertex_dummy
  in
  rem_dummy vertex_dummy hedge_dummy g;
  let (vertices,hedges) = res in
  (Sette.remove vertex_dummy vertices,
  Sette.remove hedge_dummy hedges)

(* *********************************************************************** *)
(* Strongly Connected Components *)
(* *********************************************************************** *)

let cfc_aux
  priority
  g root
  =
  let
    num     = ref(-1) and
    partition = ref [] and
    pile    = Stack.create() and
    hash    = Hashhe.create 23 and
    filterp  = filter_priority priority and
    comparep = compare_priority priority
  in
  let rec visit sommet
    =
    Stack.push sommet pile;
    incr num;
    let num_sommet = !num in
    Hashhe.replace hash sommet num_sommet;
    let succhedges = succhedge g sommet in
    let head =
      match priority with
      | All ->
	  Sette.fold compute_head succhedges !num
      | Filter _ ->
	  Sette.fold
	  (begin fun h head ->
	    if filterp h then compute_head h head else head
	  end)
	  succhedges !num
      | Priority _ ->
	  let lhedges =
	    Sette.fold
	      (begin fun h res -> if filterp h then h::res else res end)
	      succhedges []
	  in
	  let lhedges = List.sort comparep lhedges in
	  List.fold_left (fun head h -> compute_head h head) !num lhedges
    in
    if head = num_sommet then
      begin
	let element = ref (Stack.pop pile) in
	let composante = ref [!element] in
	Hashhe.replace hash !element max_int;
	while !element <> sommet do
	  element := Stack.pop pile;
	  Hashhe.replace hash !element max_int;
	  composante := !element :: !composante
	done;
	partition := !composante :: !partition
      end;
    head
  and compute_head h head
    =
    let hedge_n = hedge_n g h in
    let cond =
      array_exists
	(fun v -> Hashhe.find hash v > min_int)
	hedge_n.predvertex
    in
    if cond then begin
      Array.fold_left
	(begin fun head succ ->
	  let dfn = Hashhe.find hash succ in
	  let m = if dfn=min_int then (visit succ) else dfn in
	  min m head
	end)
	head
	hedge_n.succvertex
    end
    else
      head
  in

  iter_vertex g (fun v _ ~pred:_ ~succ:_ -> Hashhe.add hash v min_int);
  let _ = visit root in
  !partition

let cfc g root = cfc_aux All g root

let cfc_multi_aux priority vertex_dummy hedge_dummy g root =
  add_dummy_forward vertex_dummy hedge_dummy g root;
  let res = cfc_aux priority g vertex_dummy in
  rem_dummy vertex_dummy hedge_dummy g;
  begin match res with
  | (x::r)::l when (compare x vertex_dummy)=0 -> r::l
  | _ -> failwith "cfc_multi_aux"
  end

let cfc_multi vertex_dummy hedge_dummy g root =
  cfc_multi_aux All vertex_dummy hedge_dummy g root

let cfc_filter_multi vertex_dummy hedge_dummy g filter root =
  cfc_multi_aux
    (Filter (fun h -> if h = hedge_dummy then true else filter h))
    vertex_dummy hedge_dummy g root

let cfc_priority_multi vertex_dummy hedge_dummy g priority root =
  cfc_multi_aux
    (Priority (fun h -> if h = hedge_dummy then 0 else priority h))
    vertex_dummy hedge_dummy g root

(* *********************************************************************** *)
(* Strongly Connected Sub-Components *)
(* *********************************************************************** *)
let scfc_aux
  priority
  g
  root
  =
  let num     = ref(-1) and
    pile    = Stack.create() and
    hash    = Hashhe.create (size_vertex g) and
    filterp  = filter_priority priority and
    comparep = compare_priority priority
  in
  let rec composante_aux partition h
    =
    let hedge_n = hedge_n g h in
    let cond =
      array_exists
	(fun v -> Hashhe.find hash v > min_int)
	hedge_n.predvertex
    in
    if cond then begin
      Array.iter
	(begin fun succ ->
	  if Hashhe.find hash succ = min_int then begin
	    ignore (visit succ partition)
	  end
	end)
	hedge_n.succvertex
    end
  and composante sommet
    =
    let partition = ref Ilist.Nil in
    let succhedges = succhedge g sommet in
    begin match priority with
    | All ->
	Sette.iter (composante_aux partition) succhedges
    | Filter _ ->
	Sette.iter
	(begin fun h ->
	  if filterp h then composante_aux partition h
	end)
	succhedges
    | Priority _ ->
	let lhedges =
	  Sette.fold
	    (begin fun h res -> if filterp h then h::res else res end)
	    succhedges []
	in
	let lhedges = List.sort comparep lhedges in
	List.iter (composante_aux partition) lhedges
    end;
    Ilist.Cons(Ilist.Atome sommet, !partition)

  and visit sommet partition
    =
    Stack.push sommet pile;
    incr num;
    let num_sommet = !num in
    Hashhe.replace hash sommet num_sommet;
    let head = ref !num and loop = ref false in
    let succhedges = succhedge g sommet in
    begin match priority with
    | All ->
	Sette.iter (compute_head partition head loop) succhedges
    | Filter _ ->
	Sette.iter
	(begin fun h ->
	  if filterp h then (compute_head partition head loop) h
	end)
	succhedges
    | Priority _ ->
	let lhedges =
	  Sette.fold
	    (begin fun h res -> if filterp h then h::res else res end)
	    succhedges []
	in
	let lhedges = List.sort comparep lhedges in
	List.iter (compute_head partition head loop) lhedges
    end
    ;
    if !head = num_sommet then
      begin
	Hashhe.replace hash sommet max_int;
	let element = ref (Stack.pop pile) in
	if !loop then
	  begin
	    while !element <> sommet do
	      Hashhe.replace hash !element min_int;
	      element := Stack.pop pile
	    done;
	    partition := Ilist.Cons(Ilist.List(composante sommet),!partition )
	  end
	else
	  partition := Ilist.Cons(Ilist.Atome(sommet),!partition)
      end;
    !head

  and compute_head partition head loop h
    =
    let hedge_n = hedge_n g h in
    let cond =
      array_exists
	(fun v -> Hashhe.find hash v > min_int)
	hedge_n.predvertex
    in
    if cond then begin
      Array.iter
	(begin fun succ ->
	  let dfn = Hashhe.find hash succ in
	  let m =
	    if dfn=min_int
	    then (visit succ partition)
	    else dfn
	  in
	  if m <= !head then begin loop := true; head := m end
	end)
	hedge_n.succvertex
    end
  in

  iter_vertex g (fun v _ ~pred:_ ~succ:_ -> Hashhe.add hash v min_int);
  let partition = ref Ilist.Nil in
  let _ = visit root partition in
  !partition

let scfc g root =
  scfc_aux All g root

let scfc_multi_aux priority vertex_dummy hedge_dummy g root =
  add_dummy_forward vertex_dummy hedge_dummy g root;
  let res = scfc_aux priority g vertex_dummy in
  rem_dummy vertex_dummy hedge_dummy g;
  begin match res with
  | Ilist.Cons(Ilist.Atome(x),l) when (compare x vertex_dummy) = 0 -> l
  | _ -> failwith "hGraph.ml: scfc_multi"
  end

let scfc_multi vertex_dummy hedge_dummy g root =
  scfc_multi_aux All vertex_dummy hedge_dummy g root

let scfc_filter_multi vertex_dummy hedge_dummy g filter root =
  scfc_multi_aux
    (Filter (fun h -> if h = hedge_dummy then true else filter h))
    vertex_dummy hedge_dummy g root

let scfc_priority_multi vertex_dummy hedge_dummy g priority root =
  scfc_multi_aux
    (Priority (fun h -> if h = hedge_dummy then 0 else priority h))
    vertex_dummy hedge_dummy g root

(* *********************************************************************** *)
(* Printing *)
(* *********************************************************************** *)
let print
  print_vertex print_hedge print_attrvertex print_attrhedge print_info
  formatter g
  =
  let printv v =
    let vertex_n = vertex_n g v in
    fprintf formatter
      "{ @[<hv>v = %a, attrvertex = %a,@ succhedge = %a,@ predhedge = %a@] }@ "
      print_vertex v
      print_attrvertex vertex_n.attrvertex
      (Sette.print print_hedge) vertex_n.succhedge
      (Sette.print print_hedge) vertex_n.predhedge
  and printh h =
    let hedge_n = hedge_n g h in
    fprintf formatter
      "{ @[<hv>n = %a, attrhedge = %a,@ succvertex = %a,@ predvertex = %a@] }@ "
      print_hedge h
      print_attrhedge hedge_n.attrhedge
      (Print.array print_vertex) hedge_n.succvertex
      (Print.array print_vertex) hedge_n.predvertex
  in

  (* Build the set of vertices and hedges and sort it *)
  fprintf formatter "[ @[<v>";
  let vertices =
    Hashhe.fold (fun v _ res -> Sette.add v res) g.vertex Sette.empty
  in
  Sette.iter printv vertices;
  let hedges =
    Hashhe.fold (fun n _ res -> Sette.add n res) g.hedge Sette.empty
  in
  Sette.iter printh hedges;
  fprintf formatter "info = %a" print_info g.info;

  fprintf formatter " ]@]";
  ()

let print_dot
  ?(titlestyle:string="shape=ellipse,style=bold,style=filled,fontsize=20")
  ?(vertexstyle:string="shape=box,fontsize=12")
  ?(hedgestyle:string="shape=ellipse,fontsize=12")
  ?(title:string="")
  print_vertex print_hedge print_attrvertex print_attrhedge
  fmt g
  =
  fprintf fmt "digraph G {@.  @[<v>";
  if title<>"" then
    fprintf fmt "1073741823 [%s,label=\"%s\"];@ " titlestyle title;
  Hashhe.iter
    (begin fun vertex vertex_n ->
      fprintf fmt "%a [%s,label=\"%t\"];@ "
      print_vertex vertex
      vertexstyle
      (fun fmt -> print_attrvertex fmt vertex vertex_n.attrvertex);
    end)
    g.vertex;
  Hashhe.iter
    (begin fun hedge hedge_n ->
      fprintf fmt "%a [%s,label=\"%t\"];@ "
      print_hedge hedge
      hedgestyle
      (fun fmt -> print_attrhedge fmt hedge hedge_n.attrhedge);
    end)
    g.hedge;
  Hashhe.iter
    (begin fun hedge hedge_n ->
      Array.iter
      (begin fun pred ->
	fprintf fmt "%a -> %a;@ "
	print_vertex pred print_hedge hedge
      end)
      hedge_n.predvertex;
      Array.iter
      (begin fun succ ->
	fprintf fmt "%a -> %a;@ "
	print_hedge hedge print_vertex succ
      end)
      hedge_n.succvertex
    end)
    g.hedge
  ;
  fprintf fmt "@]@.}@.";
  ()


module type T = sig
  type vertex
    (** Type of vertex identifiers *)
  type hedge
    (** Type of hyperedge identifiers *)
  val vertex_dummy : vertex
    (** A dummy (never used) value for vertex identifiers (used for the
      functions [XXX_multi]) *)
  val hedge_dummy : hedge
    (** A dummy (never used) value for hyperedge identifiers (used for
      the functions [XXX_multi]) *)
  module SetV : (Sette.S with type elt=vertex)
    (** Set module for vertices *)
  module SetH : (Sette.S with type elt=hedge)
    (** Set module for hyperedges *)
  module HashV : (Hashhe.S with type key=vertex)
    (** Hash module with vertices as keys *)
  module HashH : (Hashhe.S with type key=hedge)
    (** Hash module with hyperedges as keys *)
end

module type S = sig
  type vertex
  type hedge

  module SetV : (Sette.S with type elt=vertex)
  module SetH : (Sette.S with type elt=hedge)
  module HashV : (Hashhe.S with type key=vertex)
  module HashH : (Hashhe.S with type key=hedge)

  type ('a,'b,'c) t
    (** Type of hypergraphs, where {ul
    {- 'a : information associated to vertices}
    {- 'b : information associated to hedges}
    {- 'c : user-information associated to an hypergraph}
    }
    *)
  val create : int -> 'c -> ('a,'b,'c) t
  val clear : ('a,'b,'c) t -> unit
  val is_empty : ('a,'b,'c) t -> bool

  (** {3 Statistics} *)

  val size_vertex : ('a,'b,'c) t -> int
  val size_hedge : ('a,'b,'c) t -> int
  val size_edgevh : ('a,'b,'c) t -> int
  val size_edgehv : ('a,'b,'c) t -> int
  val size : ('a,'b,'c) t -> int * int * int * int

  (** {3 Information associated to vertives and edges} *)

  val attrvertex : ('a,'b,'c) t -> vertex -> 'a
  val attrhedge : ('a,'b,'c) t -> hedge -> 'b
  val info : ('a,'b,'c) t -> 'c

  (** {3 Membership tests} *)

  val is_vertex : ('a,'b,'c) t -> vertex -> bool
  val is_hedge : ('a,'b,'c) t -> hedge -> bool

  (** {3 Successors and predecessors} *)

  val succhedge : ('a,'b,'c) t -> vertex -> SetH.t
  val predhedge : ('a,'b,'c) t -> vertex -> SetH.t
  val succvertex : ('a,'b,'c) t -> hedge -> vertex array
  val predvertex : ('a,'b,'c) t -> hedge -> vertex array
  val succ_vertex : ('a,'b,'c) t -> vertex -> SetV.t
  val pred_vertex : ('a,'b,'c) t -> vertex -> SetV.t

  (** {3 Adding and removing elements} *)

  val add_vertex : ('a,'b,'c) t -> vertex -> 'a -> unit
  val add_hedge : ('a,'b,'c) t -> hedge -> 'b -> pred:vertex array -> succ:vertex array -> unit

  val remove_vertex : ('a,'b,'c) t -> vertex -> unit
  val remove_hedge : ('a,'b,'c) t -> hedge -> unit

  (** {3 Iterators} *)

  val iter_vertex :
    ('a,'b,'c) t ->
    (vertex -> 'a -> pred:SetH.t -> succ:SetH.t -> unit) ->
    unit
  val iter_hedge :
    ('a,'b,'c) t ->
    (hedge -> 'b -> pred:vertex array -> succ:vertex array -> unit) ->
    unit
  val fold_vertex :
    ('a,'b,'c) t ->
    (vertex -> 'a -> pred:SetH.t -> succ:SetH.t -> 'g -> 'g) ->
    'g -> 'g
  val fold_hedge :
    ('a,'b,'c) t ->
    (hedge -> 'b -> pred:vertex array -> succ:vertex array -> 'g -> 'g) ->
    'g -> 'g

  val map :
    ('a,'b,'c) t ->
    (vertex -> 'a -> pred:SetH.t -> succ:SetH.t -> 'aa) ->
    (hedge -> 'b -> pred:vertex array -> succ:vertex array -> 'bb) ->
    ('c -> 'cc) ->
    ('aa,'bb,'cc) t

  (** {3 Copy and Transpose} *)

  val copy :
    (vertex -> 'a -> 'aa) ->
    (hedge -> 'b -> 'bb) ->
    ('c -> 'cc) ->
    ('a,'b,'c) t ->
    ('aa,'bb,'cc) t
  val transpose :
    (vertex -> 'a -> 'aa) ->
    (hedge -> 'b -> 'bb) ->
    ('c -> 'cc) ->
    ('a,'b,'c) t ->
    ('aa,'bb,'cc) t

  (** {3 Algorithms} *)

  val min : ('a,'b,'c) t -> SetV.t
  val max : ('a,'b,'c) t -> SetV.t

  val topological_sort :
    ('a,'b,'c) t -> vertex -> vertex list
  val topological_sort_multi :
    ('a,'b,'c) t -> SetV.t -> vertex list
  val topological_sort_filter_multi :
    ('a,'b,'c) t -> (hedge -> bool) -> SetV.t -> vertex list

  val reachable :
    ('a,'b,'c) t -> vertex -> SetV.t * SetH.t
  val reachable_multi :
    ('a,'b,'c) t -> SetV.t -> SetV.t * SetH.t
  val reachable_filter_multi :
    ('a,'b,'c) t -> (hedge -> bool) -> SetV.t -> SetV.t * SetH.t

  val cfc :
    ('a,'b,'c) t -> vertex -> vertex list list
  val cfc_multi :
    ('a,'b,'c) t -> SetV.t -> vertex list list
  val cfc_filter_multi :
    ('a,'b,'c) t -> (hedge -> bool) -> SetV.t -> vertex list list
  val cfc_priority_multi :
    ('a,'b,'c) t -> (hedge -> int) -> SetV.t -> vertex list list

  val scfc :
    ('a,'b,'c) t -> vertex -> vertex Ilist.t
  val scfc_multi :
    ('a,'b,'c) t -> SetV.t -> vertex Ilist.t
  val scfc_filter_multi :
    ('a,'b,'c) t -> (hedge -> bool) -> SetV.t -> vertex Ilist.t
  val scfc_priority_multi :
    ('a,'b,'c) t -> (hedge -> int) -> SetV.t -> vertex Ilist.t

  (** {3 Printing} *)

  val print :
    (Format.formatter -> vertex -> unit) ->
    (Format.formatter -> hedge -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    Format.formatter -> ('a,'b,'c) t -> unit

  val print_dot :
    ?titlestyle:string ->
    ?vertexstyle:string ->
    ?hedgestyle:string ->
    ?title:string ->
    (Format.formatter -> vertex -> unit) ->
    (Format.formatter -> hedge -> unit) ->
    (Format.formatter -> vertex -> 'a -> unit) ->
    (Format.formatter -> hedge -> 'b -> unit) ->
    Format.formatter -> ('a,'b,'c) t -> unit

end

module Make(T : T) : (S with type vertex=T.vertex
			and type hedge=T.hedge
			and module SetV=T.SetV
			and module SetH=T.SetH
			and module HashV=T.HashV
			and module HashH=T.HashH) = struct

  type vertex = T.vertex
  type hedge = T.hedge
  module SetV = T.SetV
  module SetH = T.SetH
  module HashV = T.HashV
  module HashH = T.HashH

  type 'a vertex_n = {
    attrvertex : 'a;
    mutable predhedge : SetH.t;
    mutable succhedge : SetH.t;
  }
  type 'b hedge_n = {
    attrhedge : 'b;
    mutable predvertex : vertex array;
    mutable succvertex : vertex array;
  }
  type ('a, 'b, 'c) t = {
    vertex : 'a vertex_n HashV.t;
    hedge : 'b hedge_n HashH.t;
    info : 'c
  }

  let create size info = {
    vertex = HashV.create size;
    hedge = HashH.create size;
    info = info;
  }
  let clear g =
    HashV.clear g.vertex;
    HashH.clear g.hedge;
    ()

  (* *********************************************************************** *)
  (* Internal functions *)
  (* *********************************************************************** *)
  let vertex_n g v = HashV.find g.vertex v
  let hedge_n g n = HashH.find g.hedge n
  let info g = g.info

  (* *********************************************************************** *)
  (* Information *)
  (* *********************************************************************** *)
  let size_vertex g = HashV.length g.vertex
  let size_hedge g = HashH.length g.hedge
  let size_edgevh g =
    let n = ref 0 in
    HashV.iter
      (fun _ vertex_n ->
	n := !n + SetH.cardinal vertex_n.succhedge)
      g.vertex;
    !n
  let size_edgehv g =
    let n = ref 0 in
    HashH.iter
      (fun _ hedge_n ->
	n := !n + Array.length hedge_n.succvertex)
      g.hedge;
    !n

  let size g =
    (size_vertex g, size_hedge g, size_edgevh g, size_edgehv g)

  (* *********************************************************************** *)
  (* Access functions *)
  (* *********************************************************************** *)
  let attrvertex g v = (vertex_n g v).attrvertex
  let attrhedge g n = (hedge_n g n).attrhedge

  (* *********************************************************************** *)
  (* Test functions *)
  (* *********************************************************************** *)
  let is_vertex g v = HashV.mem g.vertex v
  let is_hedge g n = HashH.mem g.hedge n
  let is_empty g =
    try
      HashV.iter
	(fun _ _ -> raise Exit)
	g.vertex;
      true;
    with Exit ->
      false

  (* *********************************************************************** *)
  (* Successors and predecessors *)
  (* *********************************************************************** *)
  let succhedge g v = (vertex_n g v).succhedge
  let predhedge g v = (vertex_n g v).predhedge
  let succvertex g n = (hedge_n g n).succvertex
  let predvertex g n = (hedge_n g n).predvertex

  (* *********************************************************************** *)
  (* Addition of nodes and edges *)
  (* *********************************************************************** *)
  let add_vertex g v attrvertex =
    HashV.replace g.vertex
      v { attrvertex=attrvertex; succhedge=SetH.empty; predhedge=SetH.empty }

  let add_hedge g h attrhedge ~(pred:vertex array) ~(succ:vertex array) =
    begin try
      Array.iter
	(fun v ->
	  let vertex_n =  vertex_n g v in
	  vertex_n.succhedge <- SetH.add h vertex_n.succhedge;
	)
	pred;
      Array.iter
	(fun v ->
	  let vertex_n =  vertex_n g v in
	  vertex_n.predhedge <- SetH.add h vertex_n.predhedge;
	)
	succ;
    with Not_found ->
      failwith "SHGraph.Make().add_hedge: origin vertex and/or destination hedge doesn't already exist"
    end;
    HashH.replace g.hedge
      h { attrhedge=attrhedge; predvertex=pred; succvertex=succ }

  (* *********************************************************************** *)
  (* Removal of nodes and edges *)
  (* *********************************************************************** *)

  let remove_hedge g h =
    try
      let hedge_n = hedge_n g h in
      Array.iter
	(begin fun v ->
	  let vertex_n = vertex_n g v in
	  vertex_n.succhedge <- SetH.remove h vertex_n.succhedge
	end)
	hedge_n.predvertex;
      Array.iter
	(begin fun v ->
	  let vertex_n = vertex_n g v in
	  vertex_n.predhedge <- SetH.remove h vertex_n.predhedge
	end)
	hedge_n.succvertex;
      HashH.remove g.hedge h;
    with Not_found ->
      ()

  let remove_vertex g v =
    try
      let vertex_n = vertex_n g v in
      SetH.iter (fun h -> remove_hedge g h) vertex_n.predhedge;
      SetH.iter (fun h -> remove_hedge g h) vertex_n.succhedge;
      HashV.remove g.vertex v
    with Not_found ->
      ()

  (* *********************************************************************** *)
  (* Iterators *)
  (* *********************************************************************** *)

  let iter_vertex g f =
    HashV.iter
      (fun v vertex -> f v vertex.attrvertex ~pred:vertex.predhedge ~succ:vertex.succhedge)
      g.vertex
  let iter_hedge g f =
    HashH.iter
      (fun n hedge -> f n hedge.attrhedge ~pred:hedge.predvertex ~succ:hedge.succvertex)
      g.hedge

  let fold_vertex g f res =
    HashV.fold
      (fun v vertex res -> f v vertex.attrvertex ~pred:vertex.predhedge ~succ:vertex.succhedge  res)
      g.vertex res
  let fold_hedge g f res =
    HashH.fold
      (fun n hedge res -> f n hedge.attrhedge ~pred:hedge.predvertex ~succ:hedge.succvertex res)
      g.hedge res

  let map = Obj.magic map

  (* *********************************************************************** *)
  (* Misc functions *)
  (* *********************************************************************** *)

  let succ_vertex g v =
    let succhedge = succhedge g v in
    SetH.fold
      (begin fun h res ->
	let succvertex = succvertex g h in
	Array.fold_left
	  (fun res v -> SetV.add v res)
	  res succvertex
      end)
      succhedge SetV.empty

  let pred_vertex g v =
    let predhedge = predhedge g v in
    SetH.fold
      (begin fun h res ->
	let predvertex = predvertex g h in
	Array.fold_left
	  (fun res v -> SetV.add v res)
	  res predvertex
      end)
      predhedge SetV.empty

  (* *********************************************************************** *)
  (* Copy *)
  (* *********************************************************************** *)

  let copy = Obj.magic copy

  let transpose = Obj.magic transpose

  (* *********************************************************************** *)
  (* Very internal functions *)
  (* *********************************************************************** *)

  let add_dummy_forward g (root:SetV.t) : unit =
    HashV.add g.vertex T.vertex_dummy
      { attrvertex=Obj.magic 0;
      succhedge=SetH.singleton T.hedge_dummy;
      predhedge=SetH.empty };
    HashH.add g.hedge T.hedge_dummy
      { attrhedge=Obj.magic 0;
	succvertex=begin
	  let tab = Array.make (SetV.cardinal root) (SetV.choose root) in
	  let i = ref 0 in
	  SetV.iter (fun v -> tab.(!i) <- v; incr i) root;
	  tab
	end;
	predvertex=[|T.vertex_dummy|] };
    ()

  let rem_dummy g : unit =
    HashV.remove g.vertex T.vertex_dummy;
    HashH.remove g.hedge T.hedge_dummy

  (* *********************************************************************** *)
  (* Topological sort *)
  (* *********************************************************************** *)

  let topological_sort_aux
    ?(filter=(fun _ -> true))
    (g:('a,'b,'c) t)
    (root:vertex)
    :
    vertex list
    =
    let hash = HashV.create 23 in
    let rec visit res v =
      if HashV.mem hash v then
	res
      else begin
	HashV.add hash v ();
	SetH.fold
	  (begin fun h res ->
	    if filter h then begin
	      let hedge_n = hedge_n g h in
	      let cond =
		array_forall
		  (HashV.mem hash)
		  hedge_n.predvertex
	      in
	      if cond then begin
		Array.fold_left
		  visit
		  res
		  hedge_n.succvertex
	      end
	      else res
	    end
	    else
	      res
	  end)
	  (succhedge g v)
	  (v::res)
      end
    in
    let res = visit [] root in
    List.rev res

  let topological_sort g root =
    topological_sort_aux g root

  let topological_sort_multi g root =
    add_dummy_forward g root;
    let res = topological_sort g T.vertex_dummy in
    rem_dummy g;
    List.tl res

  let topological_sort_filter_multi g filter root =
    add_dummy_forward g root;
    let res =
      topological_sort_aux
	~filter:(fun h -> if SetH.Ord.compare h T.hedge_dummy = 0 then true else filter h)
	g T.vertex_dummy
    in
    rem_dummy g;
    List.tl res

  (* *********************************************************************** *)
  (* Reachability/Coreachability *)
  (* *********************************************************************** *)
  let reachable_aux
    ?(filter=(fun _ -> true))
    (g:('a,'b,'c) t)
    (root:vertex)
    =
    let hashv = HashV.create 23 in
    let hashh = HashH.create 23 in
    let rec visit v =
      if not (HashV.mem hashv v) then begin
	HashV.add hashv v ();
	SetH.iter
	  (begin fun h ->
	    if filter h && not (HashH.mem hashh h) then begin
	      let hedge_n = hedge_n g h in
	      let cond =
		array_forall
		  (HashV.mem hashv)
		  hedge_n.predvertex
	      in
	      if cond then begin
		HashH.add hashh h ();
		Array.iter
		  visit
		  hedge_n.succvertex
	      end
	    end
	  end)
	  (succhedge g v)
      end
    in
    visit root;
    let setv =
      fold_vertex g
	(begin fun v _ ~pred:_ ~succ:_ res ->
	  if not (HashV.mem hashv v)
	  then SetV.add v res
	  else res
	end)
	SetV.empty
    and setn =
      fold_hedge g
	(begin fun h _ ~pred:_ ~succ:_ res ->
	  if not (HashH.mem hashh h)
	  then SetH.add h res
	  else res
	end)
	SetH.empty
    in
    (setv,setn)

  let reachable g root =
    reachable_aux g root

  let reachable_multi g root =
    add_dummy_forward g root;
    let res = reachable g T.vertex_dummy in
    rem_dummy g;
    let (vertices,hedges) = res in
    (SetV.remove T.vertex_dummy vertices,
    SetH.remove T.hedge_dummy hedges)

  let reachable_filter_multi g filter root =
    add_dummy_forward g root;
    let res =
      reachable_aux
	~filter:(fun h -> if SetH.Ord.compare h T.hedge_dummy = 0 then true else filter h)
	g T.vertex_dummy
    in
    rem_dummy g;
    let (vertices,hedges) = res in
    (SetV.remove T.vertex_dummy vertices,
     SetH.remove T.hedge_dummy hedges)

  (* *********************************************************************** *)
  (* Strongly Connected Components *)
  (* *********************************************************************** *)
  let cfc_aux
    priority
    (g:('a,'b,'c) t)
    (root:vertex)
    =
    let
      num     = ref(-1) and
      partition = ref [] and
      pile    = Stack.create() and
      hash    = HashV.create 23 and
      filterp  = filter_priority priority and
      comparep = compare_priority priority
    in
    let rec visit sommet
      =
      Stack.push sommet pile;
      incr num;
      let num_sommet = !num in
      HashV.replace hash sommet num_sommet;
      let succhedges = succhedge g sommet in
      let head =
	match priority with
	| All ->
	    SetH.fold compute_head succhedges !num
	| Filter _ ->
	    SetH.fold
	    (begin fun h head ->
	      if filterp h then compute_head h head else head
	    end)
	    succhedges !num
	| Priority _ ->
	    let lhedges =
	      SetH.fold
		(begin fun h res -> if filterp h then h::res else res end)
		succhedges []
	    in
	    let lhedges = List.sort comparep lhedges in
	    List.fold_left (fun head h -> compute_head h head) !num lhedges
      in
      if head = num_sommet then
	begin
	  let element = ref (Stack.pop pile) in
	  let composante = ref [!element] in
	  HashV.replace hash !element max_int;
	  while !element <> sommet do
	    element := Stack.pop pile;
	    HashV.replace hash !element max_int;
	    composante := !element :: !composante
	  done;
	  partition := !composante :: !partition
	end;
      head
    and compute_head h head
      =
      let hedge_n = hedge_n g h in
      let cond =
	array_exists
	  (fun v -> HashV.find hash v > min_int)
	  hedge_n.predvertex
      in
      if cond then begin
	Array.fold_left
	  (begin fun head succ ->
	    let dfn = HashV.find hash succ in
	    let m = if dfn=min_int then (visit succ) else dfn in
	    min m head
	  end)
	  head
	  hedge_n.succvertex
      end
      else
	head
    in

    iter_vertex g (fun v _ ~pred:_ ~succ:_ -> HashV.add hash v min_int);
    let _ = visit root in
    !partition

  let cfc g root = cfc_aux All g root

  let cfc_multi_aux priority g root =
    add_dummy_forward g root;
    let res = cfc_aux priority g T.vertex_dummy in
    rem_dummy g;
    begin match res with
    | (x::r)::l when (SetV.Ord.compare x T.vertex_dummy)=0 -> r::l
    | _ -> failwith "Make().cfc_multi_aux"
    end

  let cfc_multi g root =
    cfc_multi_aux All g root

  let cfc_filter_multi g filter root =
    cfc_multi_aux
      (Filter (fun h -> if SetH.Ord.compare h T.hedge_dummy = 0 then true else filter h))
      g root

  let cfc_priority_multi g priority root =
    cfc_multi_aux
      (Priority (fun h -> if SetH.Ord.compare h T.hedge_dummy = 0 then 0 else priority h))
      g root

  (* *********************************************************************** *)
  (* Strongly Connected Sub-Components *)
  (* *********************************************************************** *)
  let scfc_aux
    priority
    g
    root
    =
    let num     = ref(-1) and
      pile    = Stack.create() and
      hash    = HashV.create (size_vertex g) and
      filterp  = filter_priority priority and
      comparep = compare_priority priority
    in
    let rec composante_aux partition h
      =
      let hedge_n = hedge_n g h in
      let cond =
	array_exists
	  (fun v -> HashV.find hash v > min_int)
	  hedge_n.predvertex
      in
      if cond then begin
	Array.iter
	  (begin fun succ ->
	    if HashV.find hash succ = min_int then begin
	      ignore (visit succ partition)
	    end
	  end)
	  hedge_n.succvertex
      end

    and composante sommet =
      let partition = ref Ilist.Nil in
      let succhedges = succhedge g sommet in
      begin match priority with
      | All ->
	  SetH.iter (composante_aux partition) succhedges
      | Filter _ ->
	  SetH.iter
	  (begin fun h ->
	    if filterp h then composante_aux partition h
	  end)
	  succhedges
      | Priority _ ->
	  let lhedges =
	    SetH.fold
	      (begin fun h res -> if filterp h then h::res else res end)
	      succhedges []
	  in
	  let lhedges = List.sort comparep lhedges in
	  List.iter (composante_aux partition) lhedges
      end;
      Ilist.Cons(Ilist.Atome sommet, !partition)

    and visit sommet partition =
      Stack.push sommet pile;
      incr num;
      let num_sommet = !num in
      HashV.replace hash sommet num_sommet;
      let head = ref !num and loop = ref false in
      let succhedges = succhedge g sommet in
      begin match priority with
      | All ->
	  SetH.iter (compute_head partition head loop) succhedges
      | Filter _ ->
	  SetH.iter
	  (begin fun h ->
	    if filterp h then compute_head partition head loop h
	  end)
	  succhedges
      | Priority _ ->
	  let lhedges =
	    SetH.fold
	      (begin fun h res -> if filterp h then h::res else res end)
	      succhedges []
	  in
	  let lhedges = List.sort comparep lhedges in
	  List.iter (compute_head partition head loop) lhedges
      end
      ;
      if !head = num_sommet then
	begin
	  HashV.replace hash sommet max_int;
	  let element = ref (Stack.pop pile) in
	  if !loop then
	    begin
	      while !element <> sommet do
		HashV.replace hash !element min_int;
		element := Stack.pop pile
	      done;
	      partition := Ilist.Cons(Ilist.List(composante sommet),!partition )
	    end
	  else
	    partition := Ilist.Cons(Ilist.Atome(sommet),!partition)
	end;
      !head

    and compute_head partition head loop h
      =
      let hedge_n = hedge_n g h in
      let cond =
	array_exists
	  (fun v -> HashV.find hash v > min_int)
	  hedge_n.predvertex
      in
      if cond then begin
	Array.iter
	  (begin fun succ ->
	    let dfn = HashV.find hash succ in
	    let m =
	      if dfn=min_int
	      then (visit succ partition)
	      else dfn
	    in
	    if m <= !head then begin loop := true; head := m end
	  end)
	  hedge_n.succvertex
      end
    in

    iter_vertex g (fun v _ ~pred:_ ~succ:_ -> HashV.add hash v min_int);
    let partition = ref Ilist.Nil in
    let _ = visit root partition in
    !partition

  let scfc g root =
    scfc_aux All g root

  let scfc_multi_aux priority g root =
    add_dummy_forward g root;
    let res = scfc_aux priority g T.vertex_dummy in
    rem_dummy g;
    begin match res with
    | Ilist.Cons(Ilist.Atome(x),l) when (SetV.Ord.compare x T.vertex_dummy) = 0 -> l
    | _ -> failwith "Make().scfc_multi_aux"
    end

  let scfc_multi g root = scfc_multi_aux All g root

  let scfc_filter_multi g filter root =
    scfc_multi_aux
      (Filter (fun h -> if SetH.Ord.compare h T.hedge_dummy = 0 then true else filter h))
      g root

  let scfc_priority_multi g priority root =
    scfc_multi_aux
      (Priority (fun h -> if SetH.Ord.compare h T.hedge_dummy = 0 then 0 else priority h))
      g root


  (* *********************************************************************** *)
  (* Min and Max *)
  (* *********************************************************************** *)
  let min g =
    HashV.fold
      (fun v vertex_n res ->
	if vertex_n.predhedge = SetH.empty
	then SetV.add v res
	else res)
      g.vertex SetV.empty
  let max g =
    HashV.fold
      (fun v vertex_n res ->
	if vertex_n.succhedge = SetH.empty
	then SetV.add v res
	else res)
      g.vertex SetV.empty

  (* *********************************************************************** *)
  (* Printing *)
  (* *********************************************************************** *)
  let print
    print_vertex print_hedge print_attrvertex print_attrhedge print_info
    formatter g
    =
    let printv v =
      let vertex_n = vertex_n g v in
      fprintf formatter
	"{ v = %a, @[<hv>attrvertex = %a,@ succhedge = %a,@ predhedge = %a@] }@ "
	print_vertex v
	print_attrvertex vertex_n.attrvertex
	(SetH.print print_hedge) vertex_n.succhedge
	(SetH.print print_hedge) vertex_n.predhedge
    and printn n =
      let hedge_n = hedge_n g n in
      fprintf formatter
	"{ n = %a, @[<hv>attrhedge = %a,@ succvertex = %a,@ predvertex = %a@] }@ "
	print_hedge n
	print_attrhedge hedge_n.attrhedge
	(Print.array print_vertex) hedge_n.succvertex
	(Print.array print_vertex) hedge_n.predvertex
    in

    (* Build the set of vertices and hedges and sort it *)
    fprintf formatter "[ @[<v>";
    let vertices =
      HashV.fold (fun v _ res -> SetV.add v res) g.vertex SetV.empty
    in
    SetV.iter printv vertices;
    let hedges =
      HashH.fold (fun n _ res -> SetH.add n res) g.hedge SetH.empty
    in
    SetH.iter printn hedges;
    fprintf formatter "info = %a" print_info g.info;
    fprintf formatter " ]@]";
    ()

  let print_dot
    ?(titlestyle:string="shape=ellipse,style=bold,style=filled,fontsize=20")
    ?(vertexstyle:string="shape=box,fontsize=12")
    ?(hedgestyle:string="shape=ellipse,fontsize=12")
    ?(title:string="")
    print_vertex print_hedge print_attrvertex print_attrhedge
    fmt g
    =
    fprintf fmt "digraph G {@.  @[<v>";
    if title<>"" then
      fprintf fmt "1073741823 [%s,label=\"%s\"];@ " titlestyle title;
    HashV.iter
      (begin fun vertex vertex_n ->
	fprintf fmt "%a [%s,label=\"%t\"];@ "
	print_vertex vertex
	vertexstyle
	(fun fmt -> print_attrvertex fmt vertex vertex_n.attrvertex);
      end)
      g.vertex;
    HashH.iter
      (begin fun hedge hedge_n ->
	fprintf fmt "%a [%s,label=\"%t\"];@ "
	print_hedge hedge
	hedgestyle
	(fun fmt -> print_attrhedge fmt hedge hedge_n.attrhedge);
      end)
      g.hedge;
    HashH.iter
      (begin fun hedge hedge_n ->
	Array.iter
	(begin fun pred ->
	  fprintf fmt "%a -> %a;@ "
	  print_vertex pred print_hedge hedge
	end)
	hedge_n.predvertex;
	Array.iter
	  (begin fun succ ->
	    fprintf fmt "%a -> %a;@ "
	    print_hedge hedge print_vertex succ
	  end)
	  hedge_n.succvertex
      end)
      g.hedge
    ;
    fprintf fmt "@]@.}@.";
    ()
end

(* *********************************************************************** *)
(* Min and Max *)
(* *********************************************************************** *)
let min g =
  Hashhe.fold
    (fun v vertex_n res ->
	if vertex_n.predhedge = Sette.empty
	then Sette.add v res
	else res)
    g.vertex Sette.empty
let max g =
  Hashhe.fold
    (fun v vertex_n res ->
	if vertex_n.succhedge = Sette.empty
	then Sette.add v res
	else res)
    g.vertex Sette.empty
