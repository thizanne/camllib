(*i
 * $Id: listc.mli,v 1.3 2003/11/12 20:39:02 bjeannet Exp $
 *
 * Complementary functions on lists
 *
i*)

(** Complementary functions on lists *)

val rg : 'a -> 'a list -> int
val split_head_last : 'a list -> 'a list * 'a
val rem : 'a -> 'a list -> 'a list 
val remq : 'a -> 'a list -> 'a list
val remp : ('a -> bool) -> 'a list -> 'a list
val remove_duplicatas : 'a list -> 'a list
val append_without_duplicatas : 'a list -> 'a list -> 'a list
val pick : ('a -> bool) -> 'a list -> 'a
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list 
val iteri : (int -> 'a -> unit) -> 'a list -> unit
val foldi : (int -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val distribuer_operation :
  ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val inv_assoc : 'a -> ('b * 'a) list -> 'b 
val inv_mem_assoc : 'a -> ('b * 'a) list -> bool
val inv_assq : 'a -> ('b * 'a) list -> 'b
val assoc_create : 'a -> 'b -> ('a * 'b) list ref -> 'b
val pretty_print : ('a -> unit) -> 'a list -> unit
val pp_pretty_print :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val pp_pretty_print_generalized : 
    string -> string -> string -> 
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
