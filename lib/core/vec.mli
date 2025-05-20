type t = {
  x : float;
  y : float;
  z : float;
}
(** A 3D vector type *)

val make_vec3 : float -> float -> float -> t
(** [make_vec3 x y z] creates a new vector with components [x], [y], and [z] *)

val zero : t
(** [vec3_zero] is the zero vector *)

val neg : t -> t
(** [neg v] returns the negation of vector [v] *)

val tuple_of : t -> float * float * float
(** [tuple_of v] returns a tuple representation of [v]. *)

val get : t -> int -> float
(** [get v i] retrieves the [i]-th component of vector [v] *)

val set : t -> int -> float -> t
(** [set v i value] sets the [i]-th component of vector [v] to [value] *)

val add : t -> t -> t
(** [add u v] adds vectors [u] and [v] *)

val sub : t -> t -> t
(** [sub u v] subtracts vector [v] from vector [u] *)

val mul : t -> t -> t
(** [mul u v] performs element-wise multiplication of vectors [u] and [v] *)

val smul : float -> t -> t
(** [smul t v] multiplies vector [v] by scalar [t] *)

val sdiv : t -> float -> t
(** [sdiv v t] divides vector [v] by scalar [t] *)

val length : t -> float
(** [length v] computes the length (magnitude) of vector [v] *)

val length_squared : t -> float
(** [length_squared v] computes the squared length of vector [v] *)

val dot : t -> t -> float
(** [dot u v] computes the dot product of vectors [u] and [v] *)

val cross : t -> t -> t
(** [cross u v] computes the cross product of vectors [u] and [v] *)

val unit_vec : t -> t
(** [unit_vector v] normalizes vector [v] to unit length *)

val random : unit -> t
(** [random ()] samples a vector from \[0, 1\]^3 uniformly randomly. *)

val random_within : float -> float -> t
(** [random min max] samples a vector from \[min, max \]^3 uniformly randomly.
*)

val random_unit_vec : unit -> t
(** [random_unit_vec ()] returns a uniformly sampled vector from S^2. *)

val random_on_hemi : t -> t
(** [random_on_hemi normal] returns a random unit vector oriented outwards as
    determined by the normal vector [normal]. *)

val near_zero : t -> bool
(** [near_zero v] returns [true] if [v] is small enough in magnitude to cause
    floating point errors. *)

val reflect : t -> t -> t
(** [reflect v n] reflects [v] about [n]. *)

val refract : t -> t -> float -> t
(** [reflect v n etai_over_etat] refracts [v] from the normal [n] using snell's
    law. *)

val print_vec : t -> unit
(** [print_vec3 v] prints vector [v] to the console *)
