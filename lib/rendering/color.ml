open Core

type t = Vec.t

let linear_to_gamma comp = if comp > 0. then sqrt comp else 0.

(* Function to write color to an output stream *)
let write_color out pixel_color =
  let r, g, b = Vec.tuple_of pixel_color
  and clamp = Interval.make_interval 0.000 0.999 |> Interval.clamp in
  let rbyte = int_of_float (255.999 *. clamp (linear_to_gamma r))
  and gbyte = int_of_float (255.999 *. clamp (linear_to_gamma g))
  and bbyte = int_of_float (255.999 *. clamp (linear_to_gamma b)) in

  Printf.fprintf out "%d %d %d\n" rbyte gbyte bbyte
