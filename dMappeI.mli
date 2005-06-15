
(** DMappe specialized for (int,int) bindings *)

include DMappe.S with module MappeX=MappeI
		 and module MappeY=MappeI
