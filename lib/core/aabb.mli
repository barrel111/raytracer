type t = {
  x : Interval.t;
  y : Interval.t;
  z : Interval.t;
}
(** A record representing an axis-aligned bounding box (AABB). *)

val empty : t
(** An empty box. *)

val make : Interval.t -> Interval.t -> Interval.t -> t
(** [make x y z] creates a new AABB defined by the Cartesian product x times y
    times z. *)

val of_points : Vec.t -> Vec.t -> t
(** [of points p1 p2] constructs a AABB with extremal points [p1] and [p2]. *)

val axis_interval : t -> int -> Interval.t
(** [axis_interval box idx] returns the projection of [box] onto the [idx]-th
    axes as specified by-- 0th axis : x-axis, 1st axis : y-axis, 2nd axis :
    z-axis. *)

val hit : t -> Ray.t -> Interval.t -> bool
(** [hit box r i] is [true] if and only if the ray [r] within the interval [i]
    intersects the AABB [box]. *)

val of_boxes : t -> t -> t
(** [of boxes box1 box2] is the smallest AABB containing both [box1] and [box2].
*)

val comp : int -> t -> t -> int
(** [comp idx box1 box2] compares [box1] and [box2] by comparing their
    projections along the [idx]-th index. *)
