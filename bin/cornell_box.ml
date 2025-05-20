open Core
open Rendering
open Scene

let red = Materials.lambertian { x = 0.65; y = 0.05; z = 0.05 }
let white = Materials.lambertian { x = 0.73; y = 0.73; z = 0.73 }
let green = Materials.lambertian { x = 0.12; y = 0.45; z = 0.15 }
let light = Materials.diffuse_light { x = 15.; y = 50.; z = 15. }

let cam =
  Camera.init_cam 1. 800 400 75 ~vfov:40
    ~look_from:(Vec.make_vec3 278. 278. (-800.))
    ~look_at:(Vec.make_vec3 278. 278. 0.)
    ~vup:(Vec.make_vec3 0. 1. 0.) ~bg:{ x = 0.; y = 0.; z = 0. }

let world =
  let wall0 =
    Quad.make (Vec.make_vec3 555. 0. 0.) (Vec.make_vec3 0. 555. 0.)
      (Vec.make_vec3 0. 0. 555.) green
    |> Quad.bbox_hittable_of
  and wall1 =
    Quad.make (Vec.make_vec3 0. 0. 0.) (Vec.make_vec3 0. 555. 0.)
      (Vec.make_vec3 0. 0. 555.) red
    |> Quad.bbox_hittable_of
  and wall2 =
    Quad.make
      (Vec.make_vec3 113. 554. 127.)
      (Vec.make_vec3 330. 0. 0.) (Vec.make_vec3 0. 0. 305.) light
    |> Quad.bbox_hittable_of
  and wall3 =
    Quad.make (Vec.make_vec3 0. 555. 0.) (Vec.make_vec3 555. 0. 0.)
      (Vec.make_vec3 0. 0. 555.) white
    |> Quad.bbox_hittable_of
  and wall4 =
    Quad.make (Vec.make_vec3 0. 0. 0.) (Vec.make_vec3 555. 0. 0.)
      (Vec.make_vec3 0. 0. 555.) white
    |> Quad.bbox_hittable_of
  and wall5 =
    Quad.make (Vec.make_vec3 0. 0. 555.) (Vec.make_vec3 555. 0. 0.)
      (Vec.make_vec3 0. 555. 0.) white
    |> Quad.bbox_hittable_of
  and box1 =
    (World.(
       add_list
         (Quad.box
            (Vec.make_vec3 130. 0. 65.)
            (Vec.make_vec3 295. 165. 230.)
            white)
         empty
       |> bbox_hittable_of)
    |> Volume.make_volume)
      0.01 red
    |> Volume.bbox_hittable_of
  and box2 =
    World.(
      add_list
        (Quad.box
           (Vec.make_vec3 265. 0. 295.)
           (Vec.make_vec3 430. 330. 460.)
           white)
        empty
      |> bbox_hittable_of)
    (* and mirror = Quad.make (Vec.make_vec3 300. 400. 0.) (Vec.make_vec3 0. 0.
       40.) (Vec.make_vec3 0. 30. 0.) (Materials.metal (Vec.make_vec3 0.8 0.8
       0.6) 0.5) |> Quad.bbox_hittable_of *)
  in
  World.(
    empty |> add wall0 |> add wall1 |> add wall2 |> add wall3 |> add wall4
    |> add wall5 |> add box1 |> add box2)

let world_arr = world |> World.objs |> Array.of_list
let world = World.hittable_of world

let bvh_world =
  Bvh_node.(make_bvh_node world_arr 0 (Array.length world_arr) |> hittable_of)

(**[bvh_world_args lst wld] adds the objects in [lst] to [wld].*)
let rec bvh_world_args lst wld =
  match lst with
  | [] -> Bvh_node.(make_bvh_node wld 0 (Array.length wld) |> hittable_of)
  | h :: t ->
      let nwld = Array.append wld (Array.make 1 h) in
      bvh_world_args t nwld
