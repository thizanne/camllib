
(* $Id: dMappeI.ml,v 1.1 2003/11/12 21:56:37 bjeannet Exp $ *)

(** DMappe specialized for (int,int) bindings *)

module DMappeI = DMappe.Make(struct
  module MappeX = MappeI
  module MappeY = MappeI
end)

include DMappeI
