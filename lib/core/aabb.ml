open Interval
open Vec
open Ray

type t = {
  x : Interval.t;
  y : Interval.t;
  z : Interval.t;
}

let delta = 0.0001
let pad_to_min x = if Interval.size x < delta then Interval.pad x delta else x
let empty = { x = Interval.empty; y = Interval.empty; z = Interval.empty }
let make x y z = { x = pad_to_min x; y = pad_to_min y; z = pad_to_min z }

let of_points (a : Vec.t) (b : Vec.t) =
  let mk_axis axis =
    let ai = Vec.get a axis in
    let bi = Vec.get b axis in
    if ai <= bi then Interval.make_interval ai bi
    else Interval.make_interval bi ai
  in
  {
    x = mk_axis 0 |> pad_to_min;
    y = mk_axis 1 |> pad_to_min;
    z = mk_axis 2 |> pad_to_min;
  }

let axis_interval box = function
  | 0 -> box.x
  | 1 -> box.y
  | 2 -> box.z
  | _ -> invalid_arg "aabb: axis must be 0,1,2"

let hit (box : t) (ray : Ray.t) (ray_t : Interval.t) : bool =
  let rec loop axis t_acc =
    if axis = 3 then true
    else
      let iv = axis_interval box axis in
      let o = Vec.get (Ray.origin ray) axis in
      let d = Vec.get (Ray.direction ray) axis in
      let inv_d = 1.0 /. d in

      let t0 = (iv.min -. o) *. inv_d in
      let t1 = (iv.max -. o) *. inv_d in
      let t_min_axis, t_max_axis = if t0 < t1 then (t0, t1) else (t1, t0) in

      let t_min' = if t_min_axis > t_acc.min then t_min_axis else t_acc.min in
      let t_max' = if t_max_axis < t_acc.max then t_max_axis else t_acc.max in

      if t_max' <= t_min' then false
      else loop (axis + 1) { Interval.min = t_min'; max = t_max' }
  in
  loop 0 ray_t

let of_boxes a b =
  {
    x = Interval.enclosure a.x b.x;
    y = Interval.enclosure a.y b.y;
    z = Interval.enclosure a.z b.z;
  }

let comp idx a b =
  let a_axis_interval = axis_interval a idx in
  let b_axis_interval = axis_interval b idx in
  Float.compare a_axis_interval.min b_axis_interval.min
