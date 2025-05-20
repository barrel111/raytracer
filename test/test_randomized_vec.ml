open OUnit2
open Core.Vec

let tolerance = 1e-3

(**[mult_and_divide lst] creates a vector from the first three elements in [lst]
   and checks that the following equational specification holds: v + (-v) = 0.
   Requires: [lst] has exactly 3 elements.*)
let add_to_zero lst =
  match lst with
  | [ h1; h2; h3 ] ->
      let v = make_vec3 h1 h2 h3 in
      let neg_v = neg v in
      add v neg_v = zero
  | _ -> false

(**[mult_and_divide lst] creates a vector from the first three elements in [lst]
   and a scalar [s] from the last element, and checks that the following
   equational specification holds: (1/s) * v = v/s. Requires: [lst] has exactly
   4 elements.*)
let mult_and_divide lst =
  match lst with
  | [ h1; h2; h3; s ] ->
      if s > 0.000001 then
        smul (1. /. s) (make_vec3 h1 h2 h3) = sdiv (make_vec3 h1 h2 h3) s
      else true
  | _ -> false

(**[normalize lst] creates a vector from the first three elements in [lst] and
   checks that the following equational specification holds: unit_vec v = v/|v|.
   Requires: [lst] has exactly 3 elements.*)
let normalize lst =
  match lst with
  | [ h1; h2; h3 ] ->
      let v = make_vec3 h1 h2 h3 in
      unit_vec v = sdiv v (length v)
  | _ -> false

(**[unit_vec lst] checks that the following equational specification holds:
   length random_unit_vec = 1*)
let unit_vec lst = length (random_unit_vec ()) -. 1. < tolerance

(**[len_and_lensq lst] creates a vector from the first three elements in [lst]
   and checks that the following equational specification holds: length_squared
   v = (length v)^2. Requires: [lst] has exactly 3 elements.*)
let len_and_lensq lst =
  match lst with
  | [ h1; h2; h3 ] ->
      let v = make_vec3 h1 h2 h3 in
      abs_float (length_squared v -. (length v *. length v)) < tolerance
  | _ -> false

(**[dot_id_1 lst] creates 2 vectors from [lst] (labeled [v1] which consists of
   the first 3 elements of [lst] and [v2] which consists of the last 3 elements
   of [lst]), as well as 2 scalars [s1] and [s2], and checks that the following
   equational specification holds: (s1 * v1) dot (s2 * v2) = (s1*s2) * (v1 dot
   v2). Requires: [lst] has exactly 8 elements.*)
let dot_id_1 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3; s1; s2 ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      abs_float (dot (smul s1 v1) (smul 2. v2) -. (s1 *. 2. *. dot v1 v2))
      < tolerance
  | _ -> false

(**[dot_id_2 lst] creates 2 vectors from [lst] (labeled [v1] which consists of
   the first 3 elements of [lst] and [v2] which consists of the last 3 elements
   of [lst]) and checks that the following equational specification holds: |v1
   dot v2| \leq |v1||v2|. Requires: [lst] has exactly 6 elements.*)
let dot_id_2 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3 ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      dot v1 v2 <= length v1 *. length v2
  | _ -> false

(**[dot_id_3 lst] creates a vector from the first three elements in [lst] and
   checks that the following equational specification holds: v dot v = |v|^2.
   Requires: [lst] has exactly 3 elements.*)
let dot_id_3 lst =
  match lst with
  | [ h1; h2; h3 ] ->
      let v = make_vec3 h1 h2 h3 in
      abs_float (dot v v -. (length v *. length v)) < tolerance
  | _ -> false

(**[dot_id_4 lst] creates 2 vectors from [lst] (labeled [v1] which consists of
   the first 3 elements of [lst] and [v2] which consists of the last 3 elements
   of [lst]) and checks that the following equational specification holds: v1
   dot v2 = v2 dot v1. Requires: [lst] has exactly 6 elements.*)
let dot_id_4 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3 ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      abs_float (dot v1 v2 -. dot v2 v1) < tolerance
  | _ -> false

(**[dot_id_5 lst] creates 3 vectors from [lst] (labeled [v1] which consists of
   the first 3 elements of [lst],[v2] which consists of the next 3 elements of
   [lst], and [v3] which consists of the last 3 elements of [lst]) and checks
   that the following equational specification holds: (v1 + v2) dot v3 = (v1 dot
   v3) + (v2 dot v3). Requires: [lst] has exactly 9 elements.*)
let dot_id_5 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3; g1; g2; g3 ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      let v3 = make_vec3 g1 g2 g3 in
      abs_float (dot (add v1 v2) v3 -. (dot v1 v3 +. dot v2 v3)) < tolerance
  | _ -> false

(**[dot_id_6 lst] creates 3 vectors from [lst] (labeled [v1] which consists of
   the first 3 elements of [lst],[v2] which consists of the next 3 elements of
   [lst], and [v3] which consists of the last 3 elements of [lst]) and checks
   that the following equational specification holds: v3 dot (v1 + v2) = (v3 dot
   v1) + (v3 dot v2). Requires: [lst] has exactly 9 elements.*)
let dot_id_6 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3; g1; g2; g3 ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      let v3 = make_vec3 g1 g2 g3 in
      abs_float (dot v3 (add v1 v2) -. (dot v3 v1 +. dot v3 v2)) < tolerance
  | _ -> false

(**[cross_id_1 lst] creates 2 vectors from [lst] (labeled [v1] which consists of
   the first 3 elements of [lst] and [v2] which consists of the last 3 elements
   of [lst]) and checks that the following equational specification holds: |v1 x
   v2| \leq |v1| |v2|. Requires: [lst] has exactly 6 elements.*)
let cross_id_1 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3 ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      length (cross v1 v2) < length v1 *. length v2
  | _ -> false

(**[cross_id_2 lst] creates a vector from the first three elements in [lst] and
   checks that the following equational specification holds: v x v = 0.
   Requires: [lst] has exactly 3 elements.*)
let cross_id_2 lst =
  match lst with
  | [ h1; h2; h3 ] ->
      let v = make_vec3 h1 h2 h3 in
      near_zero (cross v v)
  | _ -> false

(**[cross_id_3 lst] creates 2 vectors from [lst] (labeled [v1] which consists of
   the first 3 elements of [lst] and [v2] which consists of the last 3 elements
   of [lst]) and checks that the following equational specification holds: v1 x
   v2 = - v2 x v1. Requires: [lst] has exactly 6 elements.*)
let cross_id_3 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3 ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      near_zero (add (cross v1 v2) (cross v2 v1))
  | _ -> false

(**[cross_id_4 lst] creates 3 vectors from [lst] (labeled [v1] which consists of
   the first 3 elements of [lst],[v2] which consists of the next 3 elements of
   [lst], and [v3] which consists of the last 3 elements of [lst]) and checks
   that the following equational specification holds: v1 x (v2 + v3) = (v1 x v2)
   + (v1 x v3). Requires: [lst] has exactly 9 elements.*)
let cross_id_4 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3; g1; g2; g3 ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      let v3 = make_vec3 g1 g2 g3 in
      near_zero (sub (cross v1 (add v2 v3)) (add (cross v1 v2) (cross v1 v3)))
  | _ -> false

(**[cross_id_5 lst] creates 3 vectors from [lst] (labeled [v1] which consists of
   the first 3 elements of [lst],[v2] which consists of the next 3 elements of
   [lst], and [v3] which consists of the last 3 elements of [lst]) and checks
   that the following equational specification holds: v1 x (v2 x v3) + v2 x (v3
   x v1) + v3 x (v1 x v2) = 0. Requires: [lst] has exactly 9 elements.*)
let cross_id_5 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3; g1; g2; g3 ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      let v3 = make_vec3 g1 g2 g3 in
      let w1 = cross v1 (cross v2 v3) in
      let w2 = cross v2 (cross v3 v1) in
      let w3 = cross v3 (cross v1 v2) in
      near_zero (add w1 (add w2 w3))
  | _ -> false

(**[cross_id_6 lst] creates 2 vectors from [lst] (labeled [v1] which consists of
   the first 3 elements of [lst] and [v2] which consists of the next 3 elements
   of [lst]) and takes [s] to be the last element of [lst] and checks that the
   following equational specification holds: (s * v1) x v2 = s * (v1 x v2).
   Requires: [lst] has exactly 7 elements.*)
let cross_id_6 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3; s ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      let w1 = cross (smul s v1) v2 in
      let w2 = smul s (cross v1 v2) in
      near_zero (sub w1 w2)
  | _ -> false

(**[triple_id_1 lst] creates 3 vectors from [lst] (labeled [v1] which consists
   of the first 3 elements of [lst],[v2] which consists of the next 3 elements
   of [lst], and [v3] which consists of the last 3 elements of [lst]) and checks
   that the following equational specification holds: v1 \dot (v2 x v3) = (v3 x
   v1) \dot v2. Requires: [lst] has exactly 9 elements.*)
let triple_id_1 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3; g1; g2; g3 ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      let v3 = make_vec3 g1 g2 g3 in
      abs_float (dot v1 (cross v2 v3) -. dot v2 (cross v3 v1)) < tolerance
  | _ -> false

(**[triple_id_2 lst] creates 3 vectors from [lst] (labeled [v1] which consists
   of the first 3 elements of [lst],[v2] which consists of the next 3 elements
   of [lst], and [v3] which consists of the last 3 elements of [lst]) and checks
   that the following equational specification holds: v1 \dot (v2 x v3) = (v1 x
   v2) \dot v3. Requires: [lst] has exactly 9 elements.*)
let triple_id_2 lst =
  match lst with
  | [ h1; h2; h3; f1; f2; f3; g1; g2; g3 ] ->
      let v1 = make_vec3 h1 h2 h3 in
      let v2 = make_vec3 f1 f2 f3 in
      let v3 = make_vec3 g1 g2 g3 in
      abs_float (dot v1 (cross v2 v3) -. dot v3 (cross v1 v2)) < tolerance
  | _ -> false

let vec_and_neg_zero_checking =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 3) (float_bound_inclusive 536870912.)))
        add_to_zero)

let vec_and_neg_zero_checking2 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 3) (float_bound_inclusive (-536870912.))))
        add_to_zero)

let scalar_checking =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 4) (float_bound_inclusive 536870912.)))
        mult_and_divide)

let length_checking =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 3) (float_bound_inclusive 536870912.)))
        normalize)

let random_unit_length_checking =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 1) (float_bound_inclusive 536870912.)))
        unit_vec)

let len_and_lensq_checking =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 3) (float_bound_inclusive 1000.)))
        len_and_lensq)

let dot_product_identity_checking1 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 8) (float_bound_inclusive 1000.)))
        dot_id_1)

let dot_product_identity_checking2 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 6) (float_bound_inclusive 1000.)))
        dot_id_2)

let dot_product_identity_checking3 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 3) (float_bound_inclusive 1000.)))
        dot_id_3)

let dot_product_identity_checking4 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 6) (float_bound_inclusive 1000.)))
        dot_id_4)

let dot_product_identity_checking5 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 9) (float_bound_inclusive 1000.)))
        dot_id_5)

let dot_product_identity_checking6 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 9) (float_bound_inclusive 1000.)))
        dot_id_6)

let cross_product_identity_checking1 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 6) (float_bound_inclusive 1000.)))
        cross_id_1)

let cross_product_identity_checking2 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 3) (float_bound_inclusive 1000.)))
        cross_id_2)

let cross_product_identity_checking3 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 6) (float_bound_inclusive 1000.)))
        cross_id_3)

let cross_product_identity_checking4 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 9) (float_bound_inclusive 1000.)))
        cross_id_4)

let cross_product_identity_checking5 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 9) (float_bound_inclusive 100.)))
        cross_id_5)

let cross_product_identity_checking6 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 7) (float_bound_inclusive 100.)))
        cross_id_6)

let triple_product_identity_checking1 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 9) (float_bound_inclusive 1000.)))
        triple_id_1)

let triple_product_identity_checking2 =
  QCheck_runner.to_ounit2_test
    QCheck.(
      Test.make ~count:1000
        (make Gen.(list_size (return 9) (float_bound_inclusive 1000.)))
        triple_id_2)

let suite =
  "Randomized Vec Tests"
  >::: [
         vec_and_neg_zero_checking;
         vec_and_neg_zero_checking2;
         scalar_checking;
         length_checking;
         random_unit_length_checking;
         len_and_lensq_checking;
         dot_product_identity_checking1;
         dot_product_identity_checking2;
         dot_product_identity_checking3;
         dot_product_identity_checking4;
         dot_product_identity_checking5;
         dot_product_identity_checking6;
         cross_product_identity_checking1;
         cross_product_identity_checking2;
         cross_product_identity_checking3;
         cross_product_identity_checking4;
         cross_product_identity_checking5;
         cross_product_identity_checking6;
         triple_product_identity_checking1;
         triple_product_identity_checking2;
       ]

let () = run_test_tt_main suite
