type t = {
  p : Vec.t;
  normal : Vec.t;
  front_face : bool;
}

let empty = { p = Vec.zero; normal = Vec.zero; front_face = false }

let set_face_normal r outward_normal geo =
  let front_face = Vec.dot (Ray.direction r) outward_normal < 0.0 in
  {
    geo with
    front_face;
    normal = (if front_face then outward_normal else Vec.neg outward_normal);
  }
