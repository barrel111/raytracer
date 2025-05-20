open Printf
open Core
open Rendering

type t = {
  aspect_ratio : float;
  bg : Color.t;
  cam_center : Vec.t;
  img_width : int;
  img_height : int;
  max_depth : int;
  origin_loc : Vec.t;
  pixel_delta_u : Vec.t;
  pixel_delta_v : Vec.t;
  samples_per_px : int;
}

let rec ray_color bg (obj_hit : Hittable.t) depth (r : Ray.t) : Color.t =
  if depth <= 0 then Vec.zero
  else
    match obj_hit r (Interval.make_interval 0.001 Float.infinity) with
    | None ->
        bg
        (* gradient *)
        (* let unit_direction = Vec.unit_vec (Ray.direction r) in *)
        (* let a = 0.5 *. (unit_direction.y +. 1.0) in *)
        (* Vec.add *)
        (*   (Vec.smul (1.0 -. a) (Vec.make_vec3 1.0 1.0 1.0)) *)
        (*   (Vec.smul a (Vec.make_vec3 0.5 0.7 1.0)) *)
    | Some { geo; mat } -> (
        match mat.scatter r geo with
        | Some (r_out, atten) ->
            ray_color bg obj_hit (depth - 1) r_out |> Vec.mul atten
        | None -> mat.emitted geo)

let init_cam aspect_ratio img_width samples_per_px ?(vfov = 90)
    ?(look_from = Vec.make_vec3 0. 0. 0.) ?(look_at = Vec.make_vec3 0. 0. (-1.))
    ?(vup = Vec.make_vec3 0. 1. 0.) ?(bg : Color.t = { x = 1.; y = 1.; z = 1. })
    max_depth =
  let vfovr = float_of_int vfov *. (Float.pi /. 180.) in
  let h = tan (vfovr /. 2.) in
  let img_height = int_of_float (float_of_int img_width /. aspect_ratio)
  and focal_length = Vec.sub look_from look_at |> Vec.length in
  let viewport_height = 2.0 *. h *. focal_length in
  let viewport_width = viewport_height *. aspect_ratio in
  let w = Vec.unit_vec (Vec.sub look_from look_at) in
  let u = Vec.unit_vec (Vec.cross vup w) in
  let v = Vec.cross w u in
  let cam_center = look_from
  and viewport_u = Vec.smul viewport_width u
  and viewport_v = Vec.smul (-1.0 *. viewport_height) v in
  let pixel_delta_u = Vec.sdiv viewport_u (float_of_int img_width)
  and pixel_delta_v = Vec.sdiv viewport_v (float_of_int img_height) in
  let viewport_upper_left_corner =
    Vec.sub cam_center
      (Vec.add (Vec.smul focal_length w)
         (Vec.add (Vec.sdiv viewport_u 2.0) (Vec.sdiv viewport_v 2.0)))
  in
  let origin_loc =
    Vec.add viewport_upper_left_corner
      (Vec.sdiv (Vec.add pixel_delta_u pixel_delta_v) 2.0)
  in
  {
    aspect_ratio;
    img_width;
    img_height;
    cam_center;
    origin_loc;
    pixel_delta_u;
    pixel_delta_v;
    samples_per_px;
    max_depth;
    bg;
  }

let get_ray cam i j =
  let sample_sq =
    Vec.make_vec3 (Random.float 1. -. 0.5) (Random.float 1. -. 0.5) 0.
  in
  let px_sample =
    cam.origin_loc
    |> Vec.add (cam.pixel_delta_u |> Vec.smul (float_of_int i +. sample_sq.x))
    |> Vec.add (cam.pixel_delta_v |> Vec.smul (float_of_int j +. sample_sq.y))
  and origin = cam.cam_center in
  Ray.make_ray origin (Vec.sub px_sample origin)

let render cam obj out =
  let rows = Array.init cam.img_height (fun j -> j)
  and cols = Array.init cam.img_width (fun i -> i)
  and samples = Array.init cam.samples_per_px (fun k -> k) in

  Array.iter
    (fun row ->
      eprintf "\rCurrent Scanline: %d" (row + 1);
      flush stderr;
      Array.iter
        (fun col ->
          let c =
            Array.fold_left
              (fun px_col _ ->
                px_col
                |> Vec.add
                     (get_ray cam col row |> ray_color cam.bg obj cam.max_depth))
              Vec.zero samples
          in
          Color.write_color out (float_of_int cam.samples_per_px |> Vec.sdiv c))
        cols;
      fprintf out "\n%!")
    rows;

  eprintf "\rDone.                 \n%!"

let img_height cam = cam.img_height
let img_width cam = cam.img_width
