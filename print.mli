(* $Id: print.mli,v 1.1 2005/06/14 14:27:40 bjeannet Exp $ *)

(** Printing functions for standard datatypes *)

val list : 
  ?first:(unit,Format.formatter,unit) format -> 
  ?sep:(unit,Format.formatter,unit) format -> 
  ?last:(unit,Format.formatter,unit) format -> 
  (Format.formatter -> 'a -> unit) -> 
  Format.formatter -> 'a list -> unit

val array : 
  ?first:(unit,Format.formatter,unit) format -> 
  ?sep:(unit,Format.formatter,unit) format -> 
  ?last:(unit,Format.formatter,unit) format -> 
  (Format.formatter -> 'a -> unit) -> 
  Format.formatter -> 'a array -> unit

val pair : 
  ?first:(unit,Format.formatter,unit) format -> 
  ?sep:(unit,Format.formatter,unit) format -> 
  ?last:(unit,Format.formatter,unit) format -> 
  (Format.formatter -> 'a -> unit) -> 
  (Format.formatter -> 'b -> unit) -> 
  Format.formatter -> 'a * 'b -> unit

val hash :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    ?firstbind:(unit, Format.formatter, unit) format ->
    ?sepbind:(unit, Format.formatter, unit) format ->
    ?lastbind:(unit, Format.formatter, unit) format ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) -> 
    Format.formatter -> ('a,'b) Hashtbl.t -> unit

val string_of_print:
  (Format.formatter -> 'a -> unit) ->
  ('a -> string) 

val print_of_string:
  ('a -> string) ->
  (Format.formatter -> 'a -> unit)

val sprintf:
  ('a, Format.formatter, unit, string) format4 -> 'a
