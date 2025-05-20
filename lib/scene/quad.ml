open Core
open Rendering

type t = {
  q : Vec.t;
  u : Vec.t;
  v : Vec.t;
  w : Vec.t;
  normal : Vec.t;
  d : float;
  mat : Materials.t;
  bbox : Aabb.t;
}

let make q u v mat =
  let bbox_diag1 = Aabb.of_points q (Vec.add q u |> Vec.add v)
  and bbox_diag2 = Aabb.of_points (Vec.add q u) (Vec.add q v)
  and n = Vec.cross u v in
  let normal = Vec.unit_vec n in
  let d = Vec.dot normal q and w = Vec.sdiv n (Vec.dot n n) in
  { q; u; v; mat; normal; d; w; bbox = Aabb.of_boxes bbox_diag1 bbox_diag2 }

let is_interior a b =
  let unit_interval = Interval.make_interval 0. 1. in
  if
    (not (Interval.contains unit_interval a))
    || not (Interval.contains unit_interval b)
  then false
  else true

let hit quad r ray_t =
  let denom = Vec.dot quad.normal (Ray.direction r) in
  if abs_float denom < 1e-8 then None
  else
    let t = (quad.d -. Vec.dot quad.normal (Ray.origin r)) /. denom in
    if not (Interval.contains ray_t t) then None
    else
      let intersection = Ray.at r t in
      let planar_hitpt_vector = Vec.sub intersection quad.q in
      let alpha = Vec.dot quad.w (Vec.cross planar_hitpt_vector quad.v)
      and beta = Vec.dot quad.w (Vec.cross quad.u planar_hitpt_vector) in
      if is_interior alpha beta then
        Some
          {
            Hittable.empty_data with
            t;
            mat = quad.mat;
            geo =
              {
                (Geometry.set_face_normal r quad.normal Hittable.empty_data.geo) with
                p = intersection;
              };
          }
      else None

let hittable_of quad = hit quad
let bbox_of quad = quad.bbox
let bbox_hittable_of quad = (bbox_of quad, hittable_of quad)

let box (p1 : Vec.t) (p2 : Vec.t) mat =
  let ext1 = Vec.make_vec3 (min p1.x p2.x) (min p1.y p2.y) (min p1.z p2.z)
  and ext2 = Vec.make_vec3 (max p1.x p2.x) (max p1.y p2.y) (max p1.z p2.z) in
  let dx = Vec.make_vec3 (ext2.x -. ext1.x) 0. 0.
  and dy = Vec.make_vec3 0. (ext2.y -. ext1.y) 0.
  and dz = Vec.make_vec3 0. 0. (ext2.z -. ext1.z) in
  [
    make (Vec.make_vec3 ext1.x ext1.y ext2.z) dx dy mat;
    make (Vec.make_vec3 ext2.x ext1.y ext2.z) (Vec.neg dz) dy mat;
    make (Vec.make_vec3 ext2.x ext1.y ext1.z) (Vec.neg dx) dy mat;
    make (Vec.make_vec3 ext1.x ext1.y ext1.z) dz dy mat;
    make (Vec.make_vec3 ext1.x ext2.y ext2.z) dx (Vec.neg dz) mat;
    make (Vec.make_vec3 ext1.x ext1.y ext1.z) dx dz mat;
  ]
  |> List.map bbox_hittable_of
