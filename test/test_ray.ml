open OUnit2
open Core.Ray
open Core.Vec

(** [assert_vec3_equal expected actual] helper function for floating-point
    comparison issues *)
let assert_vec3_equal expected actual =
  let cmp a b = a.x = b.x && a.y = b.y && a.z = b.z in
  assert_equal ~cmp expected actual

(** [test_origin input1 input2] checks origin ray function *)
let test_origin input1 input2 =
  "Origin test" >:: fun _ ->
  let ray = make_ray input1 input2 in
  assert_vec3_equal input1 (origin ray)

(** [test_direction input1 input2] checks direction functionality *)
let test_direction input1 input2 =
  "Direction test" >:: fun _ ->
  let ray = make_ray input1 input2 in
  assert_vec3_equal input2 (direction ray)

(** [test_at_zero input1 input2] checks that ray returns to origin *)
let test_at_zero input1 input2 =
  "Zero test" >:: fun _ ->
  let ray = make_ray input1 input2 in
  let point = at ray 0.0 in
  assert_vec3_equal input1 point

(** [test_at_one input1 input2] checks that ray returns to some expected point
*)
let test_at_one input1 input2 =
  "One test" >:: fun _ ->
  let ray = make_ray input1 input2 in
  let expected = add input1 input2 in
  let point = at ray 1.0 in
  assert_vec3_equal expected point

let suite =
  "Ray Tests"
  >::: [
         test_origin (make_vec3 1.0 2.0 3.0) (make_vec3 4.0 5.0 6.0);
         test_direction (make_vec3 1.0 2.0 3.0) (make_vec3 4.0 5.0 6.0);
         test_at_zero (make_vec3 1.0 2.0 3.0) (make_vec3 4.0 5.0 6.0);
         test_at_one (make_vec3 1.0 2.0 3.0) (make_vec3 4.0 5.0 6.0);
       ]

let () = run_test_tt_main suite
