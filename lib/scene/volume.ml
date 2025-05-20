open Core
open Rendering

type t = {
  boundary : Hittable.t;
  neg_inv_density : float;
  phase : Materials.t;
  bbox : Aabb.t;
}

let make_volume (bbox, boundary) density phase =
  { bbox; boundary; phase; neg_inv_density = -1. /. density }

let hit volume r (ray_t : Interval.t) : Hittable.data option =
  match volume.boundary r Interval.universe with
  | None -> None
  | Some hit_rec1 -> (
      match
        volume.boundary r
          (Interval.make_interval (hit_rec1.t +. 0.0001) Float.infinity)
      with
      | None -> None
      | Some hit_rec2 ->
          let rec1_t = if hit_rec1.t < ray_t.min then ray_t.min else hit_rec1.t
          and rec2_t =
            if hit_rec2.t > ray_t.max then ray_t.max else hit_rec2.t
          in
          if rec1_t >= rec2_t then None
          else
            let rec1_t = if rec1_t < 0. then 0. else rec1_t
            and ray_length = Ray.direction r |> Vec.length
            and hit_distance =
              volume.neg_inv_density *. Float.log (Random.float 1.)
            in
            let boundary_distance = (rec2_t -. rec1_t) *. ray_length in
            if hit_distance > boundary_distance then None
            else
              let rec_t = rec1_t +. (hit_distance /. ray_length) in
              Some
                {
                  t = rec_t;
                  mat = volume.phase;
                  bbox = volume.bbox;
                  geo =
                    {
                      p = Ray.at r rec_t;
                      normal = Vec.make_vec3 1. 0. 0.;
                      front_face = true;
                    };
                })

let hittable_of volume = hit volume
let bbox_of volume = volume.bbox
let bbox_hittable_of volume = (bbox_of volume, hittable_of volume)
