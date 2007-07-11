(* $Id$ *)

(* Bertrand Jeannet. This file is released under LGPL license. *)

(** Imbricated lists *)

(** The operations of this module have a functional semantics. *)

(** Type of list elements. *)
type 'a el = 
| Atome of 'a  (** Terminal case *)
| List of 'a t (** The element is recursively a list. *) 

(** Type of imbricated lists. *)
and 'a t = 
  | Nil                  (** Empty list *) 
  | Cons of 'a el * 'a t (** Non-empty list *)

val cons : 'a el -> 'a t -> 'a t
  (** Adding a new list element at the begining of the list *)
val atome : 'a -> 'a el
  (** Create a list element from a single element. *)
val list : 'a t -> 'a el
  (** Create a list element from a list. *)

val of_list : 'a list -> 'a t
  (** Create a recursive list from a list *)

val hd : 'a t -> 'a el
  (** Return the head of the list. *)
val tl : 'a t -> 'a t
  (** Return the tail of the list. *)
val length : 'a t -> int
  (** Return the ength of the list. *)
val depth : 'a t -> int
  (** Return the (maximal) depth of the list.
    - [depth [] = 0]
    - [depth [a;b;c] = 1]
    - [depth [[a];b] = 2] *)

val append : 'a t -> 'a t -> 'a t
  (** Append two lists *)
val concat : 'a t -> 'a list
  (** Flatten the recursive list and converts it to a list *)

val flatten : ?depth:int -> 'a t -> 'a t
  (** Flatten the recursive list, but only starting from the given
    depth. Defaut depth is 1. 
    - [flatten [] = []]
    - [flatten [a;[b;[c];d];e;[f]] = [a;b;c;d;e;f]]
    - [flatten ~depth:2 [a;[b;[c];d];e;[f]] = [a;[b;c;d];e;[f]]]
    - [flatten ~depth:3 [a;[b;[c];d];e;[f]] = [a;[b;[c];d];e;[f]]]
  *)

val rev : 'a t -> 'a t
  (** Recursively reverse the recursive list 
    - [rev [a;[b;[c];d];e;[f]] = [[f];e;[d;[c];b];a]]
  *)
val mem : 'a -> 'a t -> bool
  (** Membership test. *)
val map : (bool -> 'a -> 'b) -> 'a t -> 'b t
  (** Ordinary map function. 
    The boolean value indicates whether the element is beginning 
    a recursive list. *)
val iter : (bool -> 'a -> unit) -> 'a t -> unit
  (** Ordinary iteration function. 
    The boolean value indicates whether the element is beginning 
    a recursive list. *)
val fold_left : ('a -> bool -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Ordinary fold function, from left to right. *)
val iter_rec : ('a -> ('a Sette.t) array -> unit) -> 'a t -> unit
  (** Recursive iteration function, rather complex. [iter_rec f ilist] applies
    [f] to each atom of the recursive list, together with the array of
    enclosing components. When [f] is called with [f obj array], [array] is
    the array of enclosing components to which [obj] belongs, from the deepest
    to the toplevel. Each component has been removed from the elements of the
    components of the level below. 

    Example: [iter_rec f [a;[b;c;d];e;[f]]]
    is equivalent to [f a [|\{b,c,d,e,f\}|]; f b [|\{c,d\};\{a,e,f\}|]; 
    f c [|\{b,d\};\{a,e,f\}|]; f d [|\{b,c\};\{a,e,f\}|]; f e [|\{a,b,c,d,f\}|]; 
    f f [|\{\};\{a,b,c,d,e\}|]; ()]. 

*)

val print : 
  ?first:(unit, Format.formatter, unit) format ->
  ?sep:(unit, Format.formatter, unit) format ->
  ?last:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** Printing function. *)
