
(* $Id: dMappeIS.ml,v 1.1 2005/06/14 14:27:39 bjeannet Exp $ *)

(** DMappe specialized for (int,Symbol.t) bindings *)

module DMappeIS = DMappe.Make(struct
  module MappeX = MappeI
  module MappeY = Symbol.Map
end)

include DMappeIS
