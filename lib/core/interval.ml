type t = {
  min : float;
  max : float;
}
(** A type representing an interval [min, max]. *)

let empty = { min = infinity; max = neg_infinity }
let universe = { min = neg_infinity; max = infinity }
let make_interval min max = { min; max }
let size i = if i.max < i.min then 0. else i.max -. i.min
let contains i x = i.min <= x && x <= i.max
let interior i x = i.min < x && x < i.max
let clamp i x = if x < i.min then i.min else if x > i.max then i.max else x

let pad i di =
  let r = di /. 2. in
  make_interval (i.min -. r) (i.max +. r)

let enclosure x y =
  {
    min = (if x.min >= y.min then y.min else x.min);
    max = (if x.max >= y.max then x.max else y.max);
  }
