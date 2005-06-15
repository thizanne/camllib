(* $Id: statistic.mli,v 1.2 2003/11/12 20:39:03 bjeannet Exp $ *)

(** Standard statistics functions *)

val mean : float array -> float
  (** Returns the mean of the array. *)
val variance : float -> float array -> float
  (** Given the mean, returns the variance of the array. *)
val std_deviation : float -> float array -> float
  (** Given the mean, returns the standard deviation of the array. *)
