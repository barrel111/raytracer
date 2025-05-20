open Core
open Rendering

type t = {
  objs : (Aabb.t * Hittable.t) list;
  bbox : Aabb.t;
}

let empty = { objs = []; bbox = Aabb.empty }

let add obj world =
  { objs = obj :: world.objs; bbox = Aabb.of_boxes world.bbox (fst obj) }

let rec add_list objs world =
  match objs with
  | [] -> world
  | hd :: tl -> add hd world |> add_list tl

let hit_world world (r : Ray.t) (ray_t : Interval.t) : Hittable.data option =
  List.fold_left
    (fun acc (obj_hit : Aabb.t * Hittable.t) ->
      match snd acc |> (snd obj_hit) r with
      | None -> acc
      | Some data -> (Some data, Interval.make_interval ray_t.min data.t))
    (None, ray_t) world.objs
  |> fst

(* let hit_world world (r : Ray.t) (ray_t : Interval.t) : Hittable.data option =
   fst (List.fold_left (fun (closest_hit, closest_t) (obj_hit : Hittable.t) ->
   let current_interval = Interval.make_interval ray_t.min closest_t in match
   obj_hit r current_interval with | None -> (closest_hit, closest_t) | Some
   data -> (Some data, data.t)) (None, ray_t.max) world.objs) *)

let hittable_of world = hit_world world
let bbox_of world = world.bbox
let bbox_hittable_of world = (bbox_of world, hittable_of world)
let objs world = world.objs
