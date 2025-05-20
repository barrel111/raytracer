open Core
open Core
open Rendering

type t = {
  center : Vec.t;
  radius : float;
  mat : Materials.t;
  bbox : Aabb.t;
}

let make_sphere_aabb center r =
  let radvec = Vec.make_vec3 r r r in
  Aabb.of_points (Vec.sub center radvec) (Vec.add center radvec)

let make_sphere center r mat =
  { center; radius = max 0.0 r; mat; bbox = make_sphere_aabb center r }

let compute_closest_root h sqrtd a (ray_t : Interval.t) =
  let pred x = Interval.interior ray_t x |> not and root1 = (h -. sqrtd) /. a in
  if pred root1 then
    let root2 = (h +. sqrtd) /. a in
    if pred root2 then None else Some root2
  else Some root1

(* Function to check if a ray hits the sphere *)
let hit_sphere s r (ray_t : Interval.t) : Hittable.data option =
  let oc = Vec.sub s.center (Ray.origin r) in
  let a = Vec.length_squared (Ray.direction r) in
  let h = Vec.dot (Ray.direction r) oc in
  let c = Vec.dot oc oc -. (s.radius *. s.radius) in
  let discriminant = (h *. h) -. (a *. c) in

  if discriminant < 0. then None
  else
    let sqrtd = sqrt discriminant in
    match compute_closest_root h sqrtd a ray_t with
    | None -> None
    | Some t ->
        let p = Ray.at r t in
        let outward_normal = Vec.sdiv (Vec.sub p s.center) s.radius in

        Some
          (Hittable.set_face_normal r outward_normal
             { t; mat = s.mat; geo = { Geometry.empty with p }; bbox = s.bbox })

let hittable_of sphere = hit_sphere sphere
let bbox_of sphere = sphere.bbox
let bbox_hittable_of sphere = (bbox_of sphere, hittable_of sphere)
