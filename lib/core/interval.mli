type t = {
  min : float;
  max : float;
}
(** A type representing an interval of real numbers. *)

val empty : t
(** [empty] represents an empty interval. *)

val universe : t
(** [universe] represents the entire real line. *)

val make_interval : float -> float -> t
(** [make_interval min max] returns a closed interval \[min, max\]. *)

val size : t -> float
(** [size i] is the length of interval [i]. *)

val contains : t -> float -> bool
(** [contains i x] is [true] iff [x] is within the interval [i]. *)

val interior : t -> float -> bool
(** [interior i x] is [true] iff [x] is within the interval [i] but not a
    boundary point of [i]. *)

val clamp : t -> float -> float
(** [clamp i x] clamps the value [x] between [i.min] and [i.max]. *)

val pad : t -> float -> t
(** [pad i di] creates the interval \[i.min - di/2, i.max + di/2\]. *)

val enclosure : t -> t -> t
(** [enclosure x y] creates an interval that tightly encloses [x] and [y]. *)
