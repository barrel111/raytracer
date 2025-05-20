open Core
open Rendering
open Scene

let ground_mat = Materials.lambertian { x = 0.8; y = 0.8; z = 0.0 }
let sphere_mat = Materials.lambertian { x = 0.1; y = 0.2; z = 0.5 }
let sphere_emmisive = Materials.diffuse_light { x = 1.0; y = 1.0; z = 1.0 }
let lsphere_mat = Materials.metal { x = 0.8; y = 0.8; z = 0.8 } 0.3
let rsphere_mat = Materials.metal { x = 0.8; y = 0.6; z = 0.2 } 1.0

let sphere =
  Sphere.(
    make_sphere (Vec.make_vec3 0.0 0.5 (-1.2)) 0.25 sphere_emmisive
    |> bbox_hittable_of)

let metal_sphere =
  Sphere.(
    make_sphere (Vec.make_vec3 0.0 0.5 (-1.2)) 0.25 sphere_mat
    |> bbox_hittable_of)

let lsphere =
  Sphere.(
    make_sphere (Vec.make_vec3 (-1.) 0. (-1.)) 0.5 lsphere_mat
    |> bbox_hittable_of)

let rsphere =
  Sphere.(
    make_sphere (Vec.make_vec3 1. 0. (-1.)) 0.5 rsphere_mat |> bbox_hittable_of)

let ground =
  Sphere.(
    make_sphere (Vec.make_vec3 0. (-100.5) (-1.)) 100. ground_mat
    |> bbox_hittable_of)

let world =
  World.(
    empty |> add sphere |> add lsphere |> add rsphere |> add metal_sphere
    |> add ground)

let world_arr = world |> World.objs |> Array.of_list

let bvh_world =
  Bvh_node.(make_bvh_node world_arr 0 (Array.length world_arr) |> hittable_of)

let world = World.hittable_of world

let cam =
  Camera.init_cam (16. /. 9.) 600 100 50 ~vfov:90
    ~look_from:(Vec.make_vec3 (-2.) 2. 1.)
    ~look_at:(Vec.make_vec3 0. 0. (-1.))
    ~vup:(Vec.make_vec3 0. 1. 0.)
