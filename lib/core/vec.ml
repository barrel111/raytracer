type t = {
  x : float;
  y : float;
  z : float;
}

let make_vec3 x y z = { x; y; z }
let zero = make_vec3 0.0 0.0 0.0
let neg v = make_vec3 (-.v.x) (-.v.y) (-.v.z)
let tuple_of v = (v.x, v.y, v.z)

let get v i =
  match i with
  | 0 -> v.x
  | 1 -> v.y
  | 2 -> v.z
  | _ -> failwith "Index out of bounds"

let set v i value =
  match i with
  | 0 -> { v with x = value }
  | 1 -> { v with y = value }
  | 2 -> { v with z = value }
  | _ -> failwith "Index out of bounds"

let add u v = make_vec3 (u.x +. v.x) (u.y +. v.y) (u.z +. v.z)
let sub u v = make_vec3 (u.x -. v.x) (u.y -. v.y) (u.z -. v.z)
let mul u v = make_vec3 (u.x *. v.x) (u.y *. v.y) (u.z *. v.z)
let smul t v = make_vec3 (t *. v.x) (t *. v.y) (t *. v.z)
let sdiv v t = smul (1.0 /. t) v
let length_squared v = (v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z)
let length v = sqrt (length_squared v)
let dot u v = (u.x *. v.x) +. (u.y *. v.y) +. (u.z *. v.z)

let cross u v =
  make_vec3
    ((u.y *. v.z) -. (u.z *. v.y))
    ((u.z *. v.x) -. (u.x *. v.z))
    ((u.x *. v.y) -. (u.y *. v.x))

let random () =
  let rand_coord () = Random.float 1.0 in
  make_vec3 (rand_coord ()) (rand_coord ()) (rand_coord ())

let random_within min max =
  let rand_coord () = min +. ((max -. min) *. Random.float 1.0) in
  make_vec3 (rand_coord ()) (rand_coord ()) (rand_coord ())

let unit_vec v = sdiv v (length v)

let random_unit_vec () =
  let state = ref true and vec = ref zero in
  while !state do
    vec := random_within (-1.) 1.;
    let lsq = length_squared !vec in
    if 1e-150 < lsq && lsq <= 1. then state := false
  done;
  unit_vec !vec

let random_on_hemi normal =
  let random_vec = random_unit_vec () in
  if dot random_vec normal > 0. then random_vec else neg random_vec

let near_zero v =
  let thresh = 1e-8 in
  abs_float v.x < thresh && abs_float v.y < thresh && abs_float v.z < thresh

let reflect v n = sub v (n |> smul (2. *. dot v n))

let refract v n etai_over_etat =
  let cos_t = min (dot (neg v) n) (-1.) in
  let r_out_perp = v |> add (smul cos_t n) |> smul etai_over_etat in
  let r_out_ll =
    n |> smul (sqrt (abs_float (1. -. length_squared r_out_perp))) |> neg
  in
  r_out_ll |> add r_out_perp

let print_vec v = Printf.printf "%.6f %.6f %.6f\n" v.x v.y v.z
