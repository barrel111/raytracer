open OUnit2
open Core.Aabb
open Core.Interval
open Core.Vec
open Core.Ray

(** [delta] tolerance for size comparison *)
let delta = 0.001

(** [test_of_points_distinct] tests min and max of box dimensions *)
let test_of_points_distinct _ =
  let p1 = make_vec3 0.0 2.0 (-1.0) in
  let p2 = make_vec3 1.0 3.0 4.0 in
  let box = of_points p1 p2 in
  assert_equal ~printer:string_of_float 0.0 box.x.min;
  assert_equal ~printer:string_of_float 1.0 box.x.max;
  assert_equal ~printer:string_of_float 2.0 box.y.min;
  assert_equal ~printer:string_of_float 3.0 box.y.max;
  assert_equal ~printer:string_of_float (-1.0) box.z.min;
  assert_equal ~printer:string_of_float 4.0 box.z.max

(** [test_of_points_same] checks degenerate box case *)
let test_of_points_same _ =
  let p = make_vec3 5.0 (-5.0) 0.0 in
  let box = of_points p p in
  assert (delta > size box.x);
  assert (delta > size box.y);
  assert (delta > size box.z)

(** [test_of_boxes] checks bounds on union of boxes *)
let test_of_boxes _ =
  let box1 = of_points (make_vec3 0.0 0.0 0.0) (make_vec3 1.0 1.0 1.0) in
  let box2 = of_points (make_vec3 (-1.0) 2.0 0.5) (make_vec3 0.5 3.0 2.0) in
  let u = of_boxes box1 box2 in
  assert_equal ~printer:string_of_float (-1.0) u.x.min;
  assert_equal ~printer:string_of_float 1.0 u.x.max;
  assert_equal ~printer:string_of_float 0.0 u.y.min;
  assert_equal ~printer:string_of_float 3.0 u.y.max;
  assert_equal ~printer:string_of_float 0.0 u.z.min;
  assert_equal ~printer:string_of_float 2.0 u.z.max

(** [test_comp_order] tests comparison function for boxes *)
let test_comp_order _ =
  let box1 = of_points (make_vec3 0.0 0.0 0.0) (make_vec3 1.0 1.0 1.0) in
  let box2 = of_points (make_vec3 2.0 2.0 2.0) (make_vec3 3.0 3.0 3.0) in
  assert_equal (-1) (comp 0 box1 box2);
  assert_equal 1 (comp 0 box2 box1);
  assert_equal 0 (comp 0 box1 box1)

(** [test_comp_invialid] checks invalid box *)
let test_comp_invalid _ =
  let box = of_points (make_vec3 0.0 0.0 0.0) (make_vec3 1.0 1.0 1.0) in
  assert_raises (Invalid_argument "aabb: axis must be 0,1,2") (fun () ->
      comp 4 box box)

(** [test_hit_intersect] checks intersection of box and ray *)
let test_hit_intersect _ =
  let box = of_points (make_vec3 0.0 0.0 0.0) (make_vec3 1.0 1.0 1.0) in
  let ray = make_ray (make_vec3 0.5 0.5 (-1.0)) (make_vec3 0.0 0.0 1.0) in
  assert_equal true (hit box ray universe)

let suite =
  "AABB tests"
  >::: [
         "test_of_points_distinct" >:: test_of_points_distinct;
         "test_of_points_same" >:: test_of_points_same;
         "test_of_boxes" >:: test_of_boxes;
         "test_comp_order" >:: test_comp_order;
         "test_comp_invalid" >:: test_comp_invalid;
         "test_hit_intersect" >:: test_hit_intersect;
       ]

let () = run_test_tt_main suite
