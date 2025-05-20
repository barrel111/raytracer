open Core

type data = {
  geo : Geometry.t;
  t : float;
  mat : Materials.t;
  bbox : Aabb.t;
}
(** A record storing data associated with a successful hit, including the point
    [t] at which the ray intersects [bbox] along with the material [mat] of the
    intersected object and geometric data [geo]. *)

type t = Ray.t -> Interval.t -> data option
(** A type corresponding to methods capable of detecting whether a ray has hit
    an object within the specified interval of time. *)

val empty_data : data
(** [empty_data] represents the default data record associated with an
    uninitialized hit. *)

val set_face_normal : Ray.t -> Vec.t -> data -> data
(** [set_face_normal r n data] returns data with its geometric data modified to
    store the appropriately oriented normal. *)

(* val translate : Vec.t -> Aabb.t * t -> Aabb.t * t *)
(** [translate offset hittable] returns a function that corresponds to the
    object that [hittable] detects after a translation by [offset]. *)

(* val rotate : float -> Aabb.t * t -> Aabb.t * t *)
(** [rotate angle hittable] returns a function that corresponds to the object
    that [hittable] detects after rotation by [angle]. *)
