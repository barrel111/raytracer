open OUnit2
open Path_tracer
open Core.Vec
open Rendering

(** [test_write_color color expected_output] tests [color] of a ppm file against
    some [expected_output]. Note that this single test is sufficient to achieve
    high coverage on the underlying [color.ml] file. *)
let test_write_color color expected_output =
  "Color test" >:: fun _ ->
  (* temp file *)
  let filename = Filename.temp_file "test" "ppm" in
  let outc = open_out filename in
  Color.write_color outc color;
  close_out outc;
  let inc = open_in filename in
  let contents = input_line inc in
  close_in inc;
  assert_equal ~printer:(fun s -> s) expected_output contents

let tests =
  "Color tests"
  >::: [
         test_write_color { x = 1.0; y = 1.0; z = 1.0 } "255 255 255";
         test_write_color { x = 0.0; y = 0.0; z = 0.0 } "0 0 0";
         test_write_color { x = 0.5; y = 0.5; z = 0.5 } "181 181 181";
         test_write_color
           { x = 0.99999999; y = 0.9999999; z = 0.99999 }
           "255 255 255";
       ]

let () = run_test_tt_main tests
