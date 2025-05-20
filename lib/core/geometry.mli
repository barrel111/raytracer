type t = {
  p : Vec.t;
  normal : Vec.t;
  front_face : bool;
}
(** A record representing a point on a surface with its local geometric
    properties. *)

val empty : t
(** A default value for [Geometry.t]. *)

val set_face_normal : Ray.t -> Vec.t -> t -> t
(** [set_face_normal ray n geo] returns a copy of [geo] with it's [face_normal]
    field indicating whether [ray] and [n] are in different directions. In that
    case, it also appropriately updates the normal vector [n]. *)
