open Core
open Rendering

type t
(** A type representing a bounded-volume hierarchy (BVH) node. *)

val make_bvh_node : (Aabb.t * Hittable.t) Array.t -> int -> int -> t
(** [make_bvh_node objs start end] creates a BVH node containing all elements of
    [objs] from indices [start] to [end]. *)

val hittable_of : t -> Hittable.t
(** [hittable_of bvh] represents a [Hittable.t] representation of [bvh]. *)

val bbox_of : t -> Aabb.t
(** [bbox_of bvh] returns a [Aabb.t] representation of the bounding box for
    [bvh]. *)

val bbox_hittable_of : t -> Aabb.t * Hittable.t
(** [bbox_hittable_of bvh] returns a [Aab.t * Hittable.t] representation of the
    [bvh]. *)
