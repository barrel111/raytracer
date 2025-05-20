open Core
open Rendering

type t = {
  left : Hittable.t;
  right : Hittable.t;
  bbox : Aabb.t;
}

let hit node r ray_t =
  if not (Aabb.hit node.bbox r ray_t) then None
  else
    let left_hit = node.left r ray_t in
    let rhit_i =
      match left_hit with
      | Some { t } -> Interval.make_interval ray_t.min t
      | None -> ray_t
    in
    let right_hit = node.right r rhit_i in
    match right_hit with
    | Some _ -> right_hit
    | _ -> left_hit

let hittable_of node = hit node
let bbox_of node = node.bbox
let bbox_hittable_of world = (bbox_of world, hittable_of world)

let sort_sub cmp arr start_idx len =
  let arr_len = Array.length arr in
  if start_idx < 0 || len < 0 || start_idx > arr_len - len then
    invalid_arg "sort_sub: invalid start_idx or len"
  else if len > 1 then
    try
      let sub_array = Array.sub arr start_idx len in
      Array.sort cmp sub_array;
      Array.blit sub_array 0 arr start_idx len
    with Invalid_argument _ ->
      invalid_arg "sort_sub: internal error during sub/blit"

let rec make_bvh_node hittables start term =
  let axis = Random.int 3 in
  let comp = fun x y -> Aabb.comp axis (fst x) (fst y) in
  let obj_span = term - start in
  if obj_span = 1 then
    let bboxf, first = hittables.(start) in
    { left = first; right = first; bbox = bboxf }
  else if obj_span = 2 then
    let bboxf, first = hittables.(start)
    and bboxs, second = hittables.(start + 1) in
    { left = first; right = second; bbox = Aabb.of_boxes bboxf bboxs }
  else
    let () = sort_sub comp hittables start (term - start)
    and mid = start + (obj_span / 2) in
    let l = make_bvh_node hittables start mid
    and r = make_bvh_node hittables mid term in
    {
      left = l |> hittable_of;
      right = r |> hittable_of;
      bbox = Aabb.of_boxes l.bbox r.bbox;
    }
