open Core

type data = {
  geo : Geometry.t;
  t : float;
  mat : Materials.t;
  bbox : Aabb.t;
}

type t = Ray.t -> Interval.t -> data option

let empty_data =
  { geo = Geometry.empty; t = 0.; mat = Materials.empty (); bbox = Aabb.empty }

let set_face_normal r outward_normal hit_rec =
  { hit_rec with geo = Geometry.set_face_normal r outward_normal hit_rec.geo }

(* let translate offset hittable = hittable *)
(* let rotate angle hittable = hittable *)
