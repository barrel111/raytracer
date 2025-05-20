open Core
open Rendering

type t
(** A type representing a constant-density volume. *)

val make_volume : Aabb.t * Hittable.t -> float -> Materials.t -> t
(** [make_volume (bbox, hittable) density mat] creates a volume of constant
    density [density] having the material [mat] and bounds/hitting behavior
    specified by [(bbox, hittable)]. *)

val hittable_of : t -> Hittable.t
(** [hittable_of volume] returns a [Hittable.t] representation of [volume]. *)

val bbox_of : t -> Aabb.t
(** [bbox_of volume] returns a [Aabb.t] representation of the bounding box for
    [volume]. *)

val bbox_hittable_of : t -> Aabb.t * Hittable.t
(** [bbox_hittable_of volume] returns a [Aabb.t * Hittable.t] representation of
    the [volume]. *)
