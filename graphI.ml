(* $Id: graphI.ml,v 1.1 2003/11/13 16:01:33 bjeannet Exp $ *)

(** Graphs specialized for vertices of type int *)

module GraphI = Graph.Make(struct
  module MapV=MappeI
  module MapE=MappeII
end)

include GraphI
