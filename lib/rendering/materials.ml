open Core

(* type t = Ray.t -> Geometry.t -> (Ray.t * Color.t) option *)
type t = {
  scatter : Ray.t -> Geometry.t -> (Ray.t * Color.t) option;
  emitted : Geometry.t -> Color.t;
}

let empty () = { scatter = (fun _ _ -> None); emitted = (fun _ -> Vec.zero) }

let lambertian albedo : t =
  {
    scatter =
      (fun _ geo ->
        let scatter_dir = Vec.random_unit_vec () |> Vec.add geo.normal in
        let scatter_dir =
          if Vec.near_zero scatter_dir then geo.normal else scatter_dir
        in
        Some (Ray.make_ray geo.p scatter_dir, albedo));
    emitted = (fun _ -> Vec.zero);
  }

let metal albedo fuzz : t =
  let fuzz = if fuzz < 1. then fuzz else 1. in
  {
    scatter =
      (fun r_in geo ->
        let reflected = Vec.reflect (Ray.direction r_in) geo.normal in
        let reflected =
          Vec.unit_vec reflected
          |> Vec.add (Vec.random_unit_vec () |> Vec.smul fuzz)
        in
        let scattered = Ray.make_ray geo.p reflected in
        if Vec.dot (Ray.direction scattered) geo.normal > 0. then
          Some (Ray.make_ray geo.p reflected, albedo)
        else None);
    emitted = (fun _ -> Vec.zero);
  }

let uniform_diffuse atten : t =
  {
    scatter =
      (fun _ geo ->
        let reflected = Vec.random_on_hemi geo.normal
        and ones = Vec.make_vec3 1. 1. 1. in
        Some (Ray.make_ray geo.p reflected, ones |> Vec.smul atten));
    emitted = (fun _ -> Vec.zero);
  }

let dielectric ref_i : t =
  {
    scatter =
      (fun r_in geo ->
        let ri = if geo.front_face then 1.0 /. ref_i else ref_i
        and unit_dir = Vec.unit_vec (Ray.direction r_in) in
        let cos_t = min (Vec.dot (Vec.neg unit_dir) geo.normal) 1. in
        let sin_t = sqrt (1. -. (cos_t *. cos_t)) in
        let cannot_refract = ri *. sin_t > 1. in
        let reflectance cos ri =
          let r0 = Float.pow ((1. -. ri) /. (1. +. ri)) 2. in
          r0 +. ((1. -. r0) *. Float.pow (1. -. cos) 5.)
        in
        let direction =
          if cannot_refract || reflectance cos_t ri > Random.float 1. then
            Vec.reflect unit_dir geo.normal
          else Vec.refract unit_dir geo.normal ri
        in
        Some (Ray.make_ray geo.p direction, { x = 1.0; y = 1.0; z = 1.0 }));
    emitted = (fun _ -> Vec.zero);
  }

let diffuse_light emit_color =
  { scatter = (fun _ _ -> None); emitted = (fun _ -> emit_color) }

let isotropic albedo : t =
  {
    scatter =
      (fun r_in geo ->
        Some (Ray.make_ray geo.p (Vec.random_unit_vec ()), albedo));
    emitted = (fun _ -> Vec.zero);
  }
