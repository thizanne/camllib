
(** DMappe specialized for (int,Symbol.t) bindings *)

include DMappe.S with module MappeX=MappeI
		 and module MappeY=Symbol.Map

