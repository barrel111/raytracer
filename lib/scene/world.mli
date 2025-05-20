open Core
open Rendering

type t
(** Abstract type representing a collection of [Hittable.t] objects. *)

val empty : t
(** [empty] represents an empty world. *)

val add : Aabb.t * Hittable.t -> t -> t
(** [add obj world] adds [obj] to [world]. *)

val add_list : (Aabb.t * Hittable.t) list -> t -> t
(** [add_list objs world] add all the objects in [objs] to [world]. *)

val hittable_of : t -> Hittable.t
(** [hittable_of world] returns a [Hittable.t] representation of [world]. *)

val bbox_hittable_of : t -> Aabb.t * Hittable.t
(** [bbox_hittable_of world] returns a [Aabb.t * Hittable.t] representation of
    the [world]. *)

val objs : t -> (Aabb.t * Hittable.t) list
(** [objs world] returns a list of [Aabb.t * Hitttable.t] representation for
    objects contained in the [world] scene. *)
