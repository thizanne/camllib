(* $Id: print.ml,v 1.1 2005/06/14 14:27:40 bjeannet Exp $ *)

(** Printing functions for standard datatypes *)

open Format

let list
  ?(first=("[@[":(unit,Format.formatter,unit) format))
  ?(sep = (";@ ":(unit,Format.formatter,unit) format))
  ?(last = ("@]]":(unit,Format.formatter,unit) format))
  (print_elt: Format.formatter -> 'a -> unit)
  (fmt:Format.formatter)
  (list: 'a list)
  : unit
  =
  if list=[] then begin
    fprintf fmt first;
    fprintf fmt last;
  end
  else begin
    fprintf fmt first;
    let rec do_sep = function
      [e] -> print_elt fmt e
      | e::l -> (print_elt fmt e ; fprintf fmt sep; do_sep l)
    | [] -> failwith ""
    in
    do_sep list;
    fprintf fmt last;
  end

let array
  ?(first=("[|@[":(unit,Format.formatter,unit) format))
  ?(sep = (";@ ":(unit,Format.formatter,unit) format))
  ?(last = ("@]|]":(unit,Format.formatter,unit) format))
  (print_elt: Format.formatter -> 'a -> unit)
  (fmt:Format.formatter)
  (array: 'a array)
  : unit
  =
  if array=[||] then begin
    fprintf fmt first;
    fprintf fmt last;
  end
  else begin
    fprintf fmt first;
    let first = ref true in
    Array.iter
      (begin fun e ->
	if !first then first := false else fprintf fmt sep;
	print_elt fmt e
      end)
      array
    ;
    fprintf fmt last;
  end

let pair
  ?(first=("(@[":(unit,Format.formatter,unit) format))
  ?(sep = (",@ ":(unit,Format.formatter,unit) format))
  ?(last = ("@])":(unit,Format.formatter,unit) format))
  (print_elt1: Format.formatter -> 'a -> unit)
  (print_elt2: Format.formatter -> 'b -> unit)
  (fmt:Format.formatter)
  (pair: 'a*'b)
  : unit
  =
  let (e1,e2) = pair in
  fprintf fmt first;
  print_elt1 fmt e1;
  fprintf fmt sep;
  print_elt2 fmt e2;
  fprintf fmt last

let hash
  ?(first : (unit, Format.formatter, unit) format = ("[@[<hv>" : (unit, Format.formatter, unit) format))
  ?(sep : (unit, Format.formatter, unit) format = (";@ ":(unit, Format.formatter, unit) format))
  ?(last : (unit, Format.formatter, unit) format = ("@]]":(unit, Format.formatter, unit) format))
  ?(firstbind : (unit, Format.formatter, unit) format = ("" : (unit, Format.formatter, unit) format))
  ?(sepbind : (unit, Format.formatter, unit) format = (" => ":(unit, Format.formatter, unit) format))
  ?(lastbind : (unit, Format.formatter, unit) format = ("":(unit, Format.formatter, unit) format))
  (print_key:Format.formatter -> 'a -> unit)
  (print_data:Format.formatter -> 'b -> unit) 
  (formatter:Format.formatter)
  (hash:('a,'b) Hashtbl.t)
  : unit
  =
  Format.fprintf formatter first;
  let firstitem = ref true in
  Hashtbl.iter
    (begin fun key data ->
      if !firstitem then firstitem := false else Format.fprintf formatter sep;
      Format.fprintf formatter firstbind;
      print_key formatter key;
      Format.fprintf formatter sepbind;
      print_data formatter data;
      Format.fprintf formatter lastbind;
    end)
    hash;
  Format.fprintf formatter last

let print_of_string
  (string_of_a:'a -> string)
  :
  (Format.formatter -> 'a -> unit)
  =
  begin fun fmt a -> pp_print_string fmt (string_of_a a) end

let string_of_print 
  (print:Format.formatter -> 'a -> unit)
  :
  ('a -> string)
  =
  begin fun a ->
    let buffer = Buffer.create 64 in
    let formatter = Format.formatter_of_buffer buffer in
    print formatter a;
    pp_print_flush formatter ();
    let res = Buffer.contents buffer in
    Buffer.clear buffer;
    res
  end

let sprintf format =
  let buffer = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.kfprintf 
    (begin fun fmt -> 
      Format.pp_print_flush fmt ();
      let s = Buffer.contents buffer in
      Buffer.clear buffer;
      s
    end)
    fmt
    format
