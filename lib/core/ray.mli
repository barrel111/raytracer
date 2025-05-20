type t
(** An abstract type representing a ray *)

val make_ray : Vec.t -> Vec.t -> t
(** [make_ray origin direction] creates a new ray with the given origin and
    direction *)

val origin : t -> Vec.t
(** [origin ray] returns the origin of the ray *)

val direction : t -> Vec.t
(** [direction ray] returns the direction of the ray *)

val at : t -> float -> Vec.t
(** [at ray t] computes the point at parameter [t] along the ray *)
