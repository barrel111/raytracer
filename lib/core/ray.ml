type t = {
  origin : Vec.t;
  direction : Vec.t;
}

let make_ray origin direction = { origin; direction }
let origin ray = ray.origin
let direction ray = ray.direction

let at ray t =
  let point = Vec.add ray.origin (Vec.smul t ray.direction) in
  point
