open OUnit2
open Scene.World
open Rendering.Hittable
open Core.Ray
open Core.Vec
open Core.Interval

let ray = make_ray (make_vec3 0.0 0.0 0.0) (make_vec3 1.0 0.0 0.0)
let valid_interval = make_interval 0.0 Float.infinity
let empty_world = Scene.World.empty

(* Mocked Hittable.data for test hit *)
let mock_hit_at t = fun _ _ -> Some { empty_data with t }

(** [test_empty_world] tests empty world instance *)
let test_empty_world =
  "Testing Empty World" >:: fun _ ->
  let hit_func = hittable_of empty_world in
  assert_equal None (hit_func ray valid_interval)

(** [test_single_hit] tests that an object in a world detects a single hit *)
let test_single_hit =
  "Single hit" >:: fun _ ->
  let world =
    empty_world |> Scene.World.add (Core.Aabb.empty, mock_hit_at 1.0)
  in
  match hittable_of world ray valid_interval with
  | Some data -> assert_equal ~printer:string_of_float 1.0 data.t
  | None -> assert_failure "Expected hit"

let suite = "World Tests" >::: [ test_empty_world; test_single_hit ]
let () = run_test_tt_main suite
