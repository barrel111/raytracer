open OUnit2
open Core.Interval

(** [test_interval_size interval isize] verifies size function for interval
    width *)
let test_interval_size interval isize =
  "Testing size of interval" >:: fun _ -> assert_equal isize (interval |> size)

(** [test_interior interval] verifies size function for interval width *)
let test_interior interval =
  "Testing interior of interval" >:: fun _ ->
  let i = interval in
  assert_bool "boundary min" (not (interior i 1.));
  assert_bool "boundary max" (not (interior i 2.));
  assert_bool "inside" (interior i 1.5);
  assert_bool "empty interior" (not (interior empty 0.));
  assert_bool "universe interior" (interior universe 0.)

(** [test_clamp interval] clamps interval and checks bounds *)

let test_clamp interval =
  "Testing clamp" >:: fun _ ->
  let i = interval in
  assert_equal 1. (clamp i 0.5);
  assert_equal 2. (clamp i 3.);
  assert_equal 1.5 (clamp i 1.5)

let empty_tests =
  "Empty interval tests"
  >::: [
         ("Empty has size 0." >:: fun _ -> assert_equal 0. (empty |> size));
         ( "Empty contains no element" >:: fun _ ->
           assert_equal false (0. |> contains empty) );
       ]

let tests =
  "Size tests"
  >::: empty_tests
       :: [
            test_interval_size (make_interval 2. 3.) 1.;
            test_interval_size (make_interval 3. 2.) 0.;
            test_interval_size (make_interval (-5.) (-3.)) 2.;
            test_interval_size (make_interval (-3.) (-5.)) 0.;
            test_interval_size (make_interval (-1.) 1.) 2.;
            test_interior (make_interval 1. 2.);
            test_clamp (make_interval 1. 2.);
          ]

let () = run_test_tt_main tests
