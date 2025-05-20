open Core
open Rendering
open Scene

let left_red = Materials.lambertian { x = 1.0; y = 0.2; z = 0.2 }
let back_green = Materials.lambertian { x = 0.2; y = 1.0; z = 0.2 }
let right_blue = Materials.lambertian { x = 0.2; y = 0.2; z = 1.0 }
let upper_orange = Materials.lambertian { x = 1.0; y = 0.5; z = 0.0 }
let lower_teal = Materials.lambertian { x = 0.2; y = 0.8; z = 0.8 }

let cam =
  Camera.init_cam 1. 400 100 50 ~vfov:80 ~look_from:(Vec.make_vec3 0. 0. 9.)
    ~look_at:(Vec.make_vec3 0. 0. 0.) ~vup:(Vec.make_vec3 0. 1. 0.)

let world =
  let wall0 =
    Quad.make
      (Vec.make_vec3 (-3.) (-2.) 5.)
      (Vec.make_vec3 0. 0. (-4.))
      (Vec.make_vec3 0. 4. 0.) left_red
    |> Quad.bbox_hittable_of
  and wall1 =
    Quad.make
      (Vec.make_vec3 (-2.) (-2.) 0.)
      (Vec.make_vec3 4. 0. 0.) (Vec.make_vec3 0. 4. 0.) back_green
    |> Quad.bbox_hittable_of
  and wall2 =
    Quad.make
      (Vec.make_vec3 3. (-2.) 1.)
      (Vec.make_vec3 0. 0. 4.) (Vec.make_vec3 0. 4. 0.) right_blue
    |> Quad.bbox_hittable_of
  and wall3 =
    Quad.make
      (Vec.make_vec3 (-2.) 3. 1.)
      (Vec.make_vec3 4. 0. 0.) (Vec.make_vec3 0. 0. 4.) upper_orange
    |> Quad.bbox_hittable_of
  and wall4 =
    Quad.make
      (Vec.make_vec3 (-2.) (-3.) 5.)
      (Vec.make_vec3 4. 0. 0.)
      (Vec.make_vec3 0. 0. (-4.))
      lower_teal
    |> Quad.bbox_hittable_of
  in
  World.(
    empty |> add wall0 |> add wall1 |> add wall2 |> add wall3 |> add wall4
    |> hittable_of)
