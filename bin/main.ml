open Core
open Rendering
open Scene
open Unix
open Printf
open Basic_scene
open Cornell_box
open Quad_scene

let profile f : unit =
  let start_time = Unix.gettimeofday () in
  let () = f () in
  let end_time = Unix.gettimeofday () in
  Printf.printf "Total render time:\n   %.3fs\n" (end_time -. start_time);
  ()

let () =
  if Array.length Sys.argv < 3 then
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "help" then
      Printf.printf
        "Usage: %s <Output File (must end in .ppm)> <Number of Spheres, 0-2> \
         [<radius> <material, metal | diffuse | emissive> <x> <y> <z>] \
         (arguments in brackets are per sphere)>\n\
        \ When inputting x,y,z, and radius values, keep in mind that the \
         Cornell Box is 550x550x550."
        Sys.argv.(0)
    else
      print_endline
        "Please input at least two command-line arguments. For clarification \
         on the command line arguments, type \"help\"."
  else
    try
      if String.ends_with ~suffix:".ppm" Sys.argv.(1) then
        let out_file = open_out Sys.argv.(1) in
        let n_obj = int_of_string Sys.argv.(2) in
        match n_obj with
        | 0 ->
            if Array.length Sys.argv = 3 then (
              fprintf out_file "P3\n%d %d\n255\n"
                (Camera.img_width Cornell_box.cam)
                (Camera.img_height Cornell_box.cam);
              (fun () ->
                Camera.render Cornell_box.cam Cornell_box.bvh_world out_file)
              |> profile)
            else
              print_endline
                "Please provide valid input. For clarification on the command \
                 line arguments, type \"help\"."
        | 1 ->
            if Array.length Sys.argv = 8 then (
              let r1 = float_of_string Sys.argv.(3) in
              let mat1_s = Sys.argv.(4) in
              let mat1 =
                match mat1_s with
                | "metal" -> Materials.metal { x = 0.8; y = 0.8; z = 0.8 } 0.3
                | "diffuse" ->
                    Materials.lambertian { x = 0.1; y = 0.2; z = 0.5 }
                | "emissive" ->
                    Materials.diffuse_light { x = 1.0; y = 1.0; z = 1.0 }
                | _ ->
                    raise (Failure "Material is not one of the three options.")
              in
              let x1 = float_of_string Sys.argv.(5) in
              let y1 = float_of_string Sys.argv.(6) in
              let z1 = float_of_string Sys.argv.(7) in
              let s1 =
                Sphere.(
                  make_sphere (Vec.make_vec3 x1 y1 z1) r1 mat1
                  |> bbox_hittable_of)
              in
              fprintf out_file "P3\n%d %d\n255\n"
                (Camera.img_width Cornell_box.cam)
                (Camera.img_height Cornell_box.cam);
              (fun () ->
                Camera.render Cornell_box.cam
                  (Cornell_box.bvh_world_args [ s1 ] Cornell_box.world_arr)
                  out_file)
              |> profile)
            else
              print_endline
                "Please provide valid input. For clarification on the command \
                 line arguments, type \"help\"."
        | 2 ->
            if Array.length Sys.argv = 13 then (
              let r1 = float_of_string Sys.argv.(3) in
              let mat1_s = Sys.argv.(4) in
              let mat1 =
                match mat1_s with
                | "metal" -> Materials.metal { x = 0.8; y = 0.8; z = 0.8 } 0.3
                | "diffuse" ->
                    Materials.lambertian { x = 0.1; y = 0.2; z = 0.5 }
                | "emissive" ->
                    Materials.diffuse_light { x = 1.0; y = 1.0; z = 1.0 }
                | _ ->
                    raise (Failure "Material is not one of the three options.")
              in
              let x1 = float_of_string Sys.argv.(5) in
              let y1 = float_of_string Sys.argv.(6) in
              let z1 = float_of_string Sys.argv.(7) in
              let r2 = float_of_string Sys.argv.(8) in
              let mat2_s = Sys.argv.(9) in
              let mat2 =
                match mat2_s with
                | "metal" -> Materials.metal { x = 0.8; y = 0.8; z = 0.8 } 0.3
                | "diffuse" ->
                    Materials.lambertian { x = 0.1; y = 0.2; z = 0.5 }
                | "emissive" ->
                    Materials.diffuse_light { x = 1.0; y = 1.0; z = 1.0 }
                | _ ->
                    raise (Failure "Material is not one of the three options.")
              in
              let x2 = float_of_string Sys.argv.(10) in
              let y2 = float_of_string Sys.argv.(11) in
              let z2 = float_of_string Sys.argv.(12) in
              let s1 =
                Sphere.(
                  make_sphere (Vec.make_vec3 x1 y1 z1) r1 mat1
                  |> bbox_hittable_of)
              in
              let s2 =
                Sphere.(
                  make_sphere (Vec.make_vec3 x2 y2 z2) r2 mat2
                  |> bbox_hittable_of)
              in
              fprintf out_file "P3\n%d %d\n255\n"
                (Camera.img_width Cornell_box.cam)
                (Camera.img_height Cornell_box.cam);
              (fun () ->
                Camera.render Cornell_box.cam
                  (Cornell_box.bvh_world_args [ s1; s2 ] Cornell_box.world_arr)
                  out_file)
              |> profile)
            else
              print_endline
                "Please provide valid input. For clarification on the command \
                 line arguments, type \"help\"."
        | _ ->
            print_endline
              "Please provide at most 2 objects to generate in the room."
      else print_endline "The output file name must end in \".ppm\"."
    with Failure _ ->
      print_endline
        "Please provide valid input. For clarification on the command line \
         arguments, type \"help\"."

(* let out_file = open_out "output.ppm" in fprintf out_file "P3\n%d %d\n255\n"
   (Camera.img_width Cornell_box.cam) (Camera.img_height Cornell_box.cam); (fun
   () -> Camera.render Cornell_box.cam Cornell_box.bvh_world out_file) |>
   profile) *)
