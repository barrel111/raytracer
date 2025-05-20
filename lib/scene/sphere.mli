open Core
open Rendering

type t = {
  center : Vec.t;
  radius : float;
  mat : Materials.t;
  bbox : Aabb.t;
}
(** A type representing spheres, storing its center, radius, bounding-box and
    material for rendering. *)

val make_sphere : Vec.t -> float -> Materials.t -> t
(** [make_sphere center r mat] creates a sphere with center [center], radius [r]
    and material [mat]. *)

(* Function to check if a ray hits the sphere *)
val hit_sphere : t -> Ray.t -> Interval.t -> Hittable.data option
(** [hit_sphere sphere ray ray_t] returns [None] if [ray] doesn't meet the
    [sphere] or exceeds the bounds set by [ray_t]. If [ray] does intersect
    [sphere] within the bounds of [ray_t] then we return a [Hittable.data]
    (wrapped in an [option]) that provides details of the collision. *)

val hittable_of : t -> Hittable.t
(** [hittable_of sphere] returns a [Hittable.t] representation of [sphere]. *)

val bbox_of : t -> Aabb.t
(** [hittable_of sphere] returns a [Aabb.t] representation of the bounding box
    for [sphere]. *)

val bbox_hittable_of : t -> Aabb.t * Hittable.t
(** [bbox_hittable_of sphere] returns a [Aabb.t * Hittable.t] representation of
    the [sphere]. *)
