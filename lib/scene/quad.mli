open Core
open Rendering

type t
(** A type representing a quad. *)

val make : Vec.t -> Vec.t -> Vec.t -> Materials.t -> t
(** [make q u v mat] is a quad with starting corner [q], vectors [u] and [v]
    representing the first and second sides as well as an associated material
    [mat]. *)

val box : Vec.t -> Vec.t -> Materials.t -> (Aabb.t * Hittable.t) list
(** [box p1 p2 mat] returns a list of quads that corresponds to a box with
    opposite ends given by [p1] and [p2]. Rendering all surfaces of this box
    utilizes material [mat] .*)

val bbox_hittable_of : t -> Aabb.t * Hittable.t
(** [bbox_hittable_of quad] returns a [Aabb.t * Hittable.t] representation of
    the [quad]. *)

val bbox_of : t -> Aabb.t
(** [bbox_of quad] returns a [Aabb.t] representation of the bounding box for
    [quad]. *)

val hittable_of : t -> Hittable.t
(** [hittable_of quad] returns a [Hittablet.t] representation of the [quad]. *)
