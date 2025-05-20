open OUnit2
open Rendering
open Rendering.Hittable
open Core.Interval
open Core.Vec
open Core.Ray
open Scene.Sphere

(** [test_make_sphere center] checks [center] and [radius] of a sphere with
    start point [center] *)
let test_make_sphere center =
  "Sphere center test" >:: fun _ ->
  let s = make_sphere center 4.0 (Materials.empty ()) in
  assert_equal s.center center;
  assert_equal s.radius 4.0

(** [test_make_negative_sphere center] checks negative [center] points are
    clamped *)
let test_make_negative_sphere center =
  "Negative sphere center test" >:: fun _ ->
  let s = make_sphere center (-5.0) (Materials.empty ()) in
  assert_equal s.radius 0.0

(** [test_miss_sphere center] checks that a specified [ray] misses a sphere
    located at [center] *)
let test_miss_sphere center =
  "No hit test" >:: fun _ ->
  let radius = 1.0 in
  let s = make_sphere center radius (Materials.empty ()) in
  let ray_origin = make_vec3 2.0 0.0 0.0 in
  let ray_dir = make_vec3 0.0 1.0 0.0 in
  let r = make_ray ray_origin ray_dir in
  let hit = hit_sphere s r (make_interval 0.0 1000.0) in
  assert_equal hit None

(** [test_hit center] checks that a specified [ray] hits a sphere located at
    [center] *)
let test_hit center =
  "Hit test" >:: fun _ ->
  let radius = 2.0 in
  let s = make_sphere center radius (Materials.empty ()) in
  let r = make_ray (make_vec3 0.0 0.0 0.0) (make_vec3 0.0 0.0 1.0) in
  match hit_sphere s r (make_interval 0.0 1000.0) with
  | Some hit ->
      assert_equal hit.t 2.0;
      assert_equal (make_vec3 0.0 0.0 2.0) hit.geo.p;
      assert_equal (make_vec3 0.0 0.0 (-1.0)) hit.geo.normal
  | None -> assert false

let suite =
  "Sphere Tests"
  >::: [
         test_make_sphere (make_vec3 0.0 0.0 0.0);
         test_make_negative_sphere (make_vec3 0.0 0.0 0.0);
         test_miss_sphere (make_vec3 0.0 0.0 0.0);
         test_hit (make_vec3 0.0 0.0 0.0);
       ]

let () = run_test_tt_main suite
