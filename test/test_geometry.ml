open OUnit2
open Core.Geometry
open Core.Vec
open Core.Ray

(** [vec_approx_equal v1 v2] checks that [v1] and [v2] are approximately
    equivalent to each other. This is to avoid numerical imprecision errors that
    may arise. Note that This was originally written for [test_ray], but may not
    be needed. May deprecate this in the future *)
let vec_approx_equal v1 v2 =
  let epsilon = 1e-6 in
  let cmp a b = abs_float (a -. b) < epsilon in
  cmp (get v1 0) (get v2 0)
  && cmp (get v1 1) (get v2 1)
  && cmp (get v1 2) (get v2 2)

(** [vec printer v] is a helper function that formats and prints [v]. Used to
    compare against a ground truth *)
let vec_printer v =
  Printf.sprintf "Vec output: (%f, %f, %f)" (get v 0) (get v 1) (get v 2)

(** [test_empty] is the unit test of empty geometry *)
let test_empty =
  "test_empty" >:: fun _ ->
  let empty_geo = empty in
  assert_equal ~printer:vec_printer zero empty_geo.p;
  assert_equal ~printer:vec_printer zero empty_geo.normal;
  assert_bool "front_face false" (not empty_geo.front_face)

(** [test_normal_front ray] tests the normal vector from the front face of a
    geometry *)
let test_normal_front ray =
  "test_normal_front" >:: fun _ ->
  let outward_normal = make_vec3 0.0 0.0 1.0 in
  let original_geo =
    { p = make_vec3 1.0 2.0 3.0; normal = zero; front_face = false }
  in
  let new_geo = set_face_normal ray outward_normal original_geo in
  assert_equal ~printer:vec_printer original_geo.p new_geo.p;
  assert_bool "front_face true" new_geo.front_face;
  assert_equal ~cmp:vec_approx_equal ~printer:vec_printer outward_normal
    new_geo.normal

(** [test_normal_front_false] tests the normal vector from the front face of a
    geometry with no expectation of a hit *)
let test_normal_front_false =
  "test_normal_front_false" >:: fun _ ->
  let ray_dir = make_vec3 0.0 0.0 1.0 in
  let ray = make_ray (make_vec3 0.0 0.0 0.0) ray_dir in
  let outward_normal = make_vec3 0.0 0.0 1.0 in
  let original_geo =
    { p = make_vec3 1.0 2.0 3.0; normal = zero; front_face = false }
  in
  let new_geo = set_face_normal ray outward_normal original_geo in
  assert_equal ~printer:vec_printer original_geo.p new_geo.p;
  assert_bool "front_face false" (not new_geo.front_face);
  assert_equal ~cmp:vec_approx_equal ~printer:vec_printer (neg outward_normal)
    new_geo.normal

(** [test_face_normal_dot_zero] verifies that when ray direction and outward
    normal are orthogonal, the front face is set to false and the normal is
    flipped *)
let test_face_normal_dot_zero =
  "test_face_normal_dot_zero" >:: fun _ ->
  let ray_dir = make_vec3 1.0 0.0 0.0 in
  let ray = make_ray (make_vec3 0.0 0.0 0.0) ray_dir in
  let outward_normal = make_vec3 0.0 1.0 0.0 in
  let original_geo =
    { p = make_vec3 1.0 2.0 3.0; normal = zero; front_face = true }
  in
  let new_geo = set_face_normal ray outward_normal original_geo in
  assert_equal ~printer:vec_printer original_geo.p new_geo.p;
  assert_bool "front_face false" (not new_geo.front_face);
  assert_equal ~cmp:vec_approx_equal ~printer:vec_printer (neg outward_normal)
    new_geo.normal

let tests =
  "Geometry tests"
  >::: [
         test_empty;
         test_normal_front
           (make_ray (make_vec3 0.0 0.0 0.0) (make_vec3 0.0 0.0 (-1.0)));
         test_normal_front_false;
         (* we remove the [ray] argument for these two since we directly
            integrate it into the function itself *)
         test_face_normal_dot_zero;
         (* same as above *)
       ]

let () = run_test_tt_main tests
