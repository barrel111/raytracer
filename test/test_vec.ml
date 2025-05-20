open OUnit2
open Core.Vec

(** [test_make_vec3] construction test *)
let test_make_vec3 _ =
  let v = make_vec3 1.0 2.0 3.0 in
  assert_equal v.x 1.0;
  assert_equal v.y 2.0;
  assert_equal v.z 3.0

(** [test_zero] checks zero vector *)
let test_zero _ =
  assert_equal zero.x 0.0;
  assert_equal zero.y 0.0;
  assert_equal zero.z 0.0

(** [test_neg] checks vector negation *)
let test_neg _ =
  let v = make_vec3 1.0 (-2.0) 3.0 in
  let nv = neg v in
  assert_equal nv.x (-1.0);
  assert_equal nv.y 2.0;
  assert_equal nv.z (-3.0)

(** [test_tuple_of] checks tuple conversion of vector *)
let test_tuple_of _ =
  let v = make_vec3 1.0 2.0 3.0 in
  let t = tuple_of v in
  assert_equal t (1.0, 2.0, 3.0)

(** [test_get] checks getting components by index *)
let test_get _ =
  let v = make_vec3 1.0 2.0 3.0 in
  assert_equal (get v 0) 1.0;
  assert_equal (get v 1) 2.0;
  assert_equal (get v 2) 3.0;
  assert_raises (Failure "Index out of bounds") (fun () -> get v 3);
  assert_raises (Failure "Index out of bounds") (fun () -> get v (-1))

(** [test_set] checks setting components by index *)
let test_set _ =
  let v = make_vec3 1.0 2.0 3.0 in
  let v' = set v 0 4.0 in
  assert_equal v'.x 4.0;
  let v'' = set v' 1 5.0 in
  assert_equal v''.y 5.0;
  let v''' = set v'' 2 6.0 in
  assert_equal v'''.z 6.0;
  assert_raises (Failure "Index out of bounds") (fun () -> set v 3 0.0);
  assert_raises (Failure "Index out of bounds") (fun () -> set v (-1) 0.0)

(** [test_add] checks vector addition *)
let test_add _ =
  let u = make_vec3 1.0 2.0 3.0 in
  let v = make_vec3 4.0 5.0 6.0 in
  let w = add u v in
  assert_equal w.x 5.0;
  assert_equal w.y 7.0;
  assert_equal w.z 9.0

(** [test_sub] checks vector subtraction *)
let test_sub _ =
  let u = make_vec3 5.0 5.0 5.0 in
  let v = make_vec3 1.0 2.0 3.0 in
  let w = sub u v in
  assert_equal w.x 4.0;
  assert_equal w.y 3.0;
  assert_equal w.z 2.0

(** [test_mul] checks vector multiplication *)
let test_mul _ =
  let u = make_vec3 2.0 3.0 4.0 in
  let v = make_vec3 1.0 2.0 (-1.0) in
  let w = mul u v in
  assert_equal w.x 2.0;
  assert_equal w.y 6.0;
  assert_equal w.z (-4.0)

(** [test_smul] checks scalar multiplication *)
let test_smul _ =
  let v = make_vec3 1.0 2.0 3.0 in
  let scaled = smul 2.0 v in
  assert_equal scaled.x 2.0;
  assert_equal scaled.y 4.0;
  assert_equal scaled.z 6.0

(** [test_length] checks vector length operations *)
let test_length _ =
  let v = make_vec3 3.0 4.0 0.0 in
  assert_equal (length_squared v) 25.0;
  assert_equal (length v) 5.0

(** [test_dot] checks vector dot product operation *)
let test_dot _ =
  let u = make_vec3 1.0 2.0 3.0 in
  let v = make_vec3 4.0 5.0 6.0 in
  assert_equal (dot u v) 32.0

(** [test_cross] checks vector cross product *)
let test_cross _ =
  let i = make_vec3 1.0 0.0 0.0 in
  let j = make_vec3 0.0 1.0 0.0 in
  let k = cross i j in
  assert_equal k.x 0.0;
  assert_equal k.y 0.0;
  assert_equal k.z 1.0;
  let neg_k = cross j i in
  (* anti-commutativity *)
  assert_equal neg_k.z (-1.0)

(** [test_unit_vec] checks unit vector value *)
let test_unit_vec _ =
  let v = make_vec3 1.0 2.0 3.0 in
  let u = unit_vec v in
  let len = length u in
  assert_equal len 1.0 ~printer:string_of_float

(** [test_random] checks random vectors *)
let test_random _ =
  Random.init 0;
  let v = random () in
  (* selects from [0, 1] *)
  assert_bool "x in [0, 1]" (0.0 <= v.x && v.x <= 1.0);
  assert_bool "y in [0, 1]" (0.0 <= v.y && v.y <= 1.0);
  assert_bool "z in [0, 1]" (0.0 <= v.z && v.z <= 1.0)

(** [test_random_within] checks random vectors with custom bounds *)
let test_random_within _ =
  Random.init 0;
  let min = -2.0 and max = 3.0 in
  let v = random_within min max in
  assert_bool "x in [min, max]" (min <= v.x && v.x <= max);
  assert_bool "y in [min, max]" (min <= v.y && v.y <= max);
  assert_bool "z in [min, max]" (min <= v.z && v.z <= max)

(** [test_random_unit_vec] checks length of random vector *)
let test_random_unit_vec _ =
  Random.init 0;
  let v = random_unit_vec () in
  let len = length v in
  assert_equal len 1.0 ~printer:string_of_float

(** [test_random_on_hemi] checks [random_on_hemi] function such that the
    generated vector is in the same hemisphere as the normal vector *)
let test_random_on_hemi _ =
  Random.init 0;
  let normal = make_vec3 0.0 1.0 0.0 in
  let v = random_on_hemi normal in
  assert_bool "dot product positive" (dot v normal > 0.0)

(** [test_near_zero] checks vectors with respect to zero vector via [near_zero]
*)
let test_near_zero _ =
  let small = make_vec3 1e-9 1e-9 1e-9 in
  let not_small = make_vec3 1e-5 1e-5 1e-5 in
  assert_bool "small vector" (near_zero small);
  assert_bool "not small" (not (near_zero not_small))

(** [test_reflect] checks that vectors are correctly reflected *)
let test_reflect _ =
  let v = make_vec3 1.0 (-1.0) 0.0 in
  let n = make_vec3 0.0 1.0 0.0 in
  let r = reflect v n in
  assert_equal r.x 1.0;
  assert_equal r.y 1.0;
  assert_equal r.z 0.0

let suite =
  "Vec tests"
  >::: [
         "make_vec3" >:: test_make_vec3;
         "zero" >:: test_zero;
         "neg" >:: test_neg;
         "tuple_of" >:: test_tuple_of;
         "get" >:: test_get;
         "set" >:: test_set;
         "add" >:: test_add;
         "sub" >:: test_sub;
         "mul" >:: test_mul;
         "smul" >:: test_smul;
         "length" >:: test_length;
         "dot" >:: test_dot;
         "cross" >:: test_cross;
         "unit_vec" >:: test_unit_vec;
         "random" >:: test_random;
         "random_within" >:: test_random_within;
         "random_unit_vec" >:: test_random_unit_vec;
         "random_on_hemi" >:: test_random_on_hemi;
         "near_zero" >:: test_near_zero;
         "reflect" >:: test_reflect;
       ]

(* Run the test suite *)
let () = run_test_tt_main suite
