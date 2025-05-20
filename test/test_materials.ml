open OUnit2
open Core
open Core.Geometry
open Core.Ray
open Rendering
open Rendering.Materials
open Vec

(** [test_empty_material] tests empty material for no scatters rays/emitted
    light *)

let test_empty_material _ =
  let m = empty () in
  let r = Ray.make_ray Vec.zero Vec.zero in
  let geo = { p = Vec.zero; normal = Vec.zero; front_face = true } in
  assert_equal None (m.scatter r geo);
  assert_equal Vec.zero (m.emitted geo)

(** [test_lambertian_scatter] tests Lambertian material scattering *)
let test_lambertian_scatter _ =
  let albedo = Vec.make_vec3 0.8 0.5 0.2 in
  let m = lambertian albedo in
  let geo =
    { p = Vec.zero; normal = Vec.make_vec3 0.0 1.0 0.0; front_face = true }
  in

  match m.scatter (Ray.make_ray Vec.zero Vec.zero) geo with
  | Some (scattered, attenuation) ->
      assert_equal albedo attenuation;
      assert_bool "Scatter direction valid"
        (Vec.length (direction scattered) > 0.0);
      assert_bool "Direction above surface"
        (Vec.dot (direction scattered) geo.normal > 0.0)
  | None -> assert_failure "Should scatter"

(** [test_metal_reflection] tests reflection on metal objects *)
let test_metal_reflection _ =
  let m = metal (Vec.make_vec3 1.0 1.0 0.0) 0.0 in
  let incoming = Ray.make_ray Vec.zero (Vec.make_vec3 0.0 (-1.0) 0.0) in
  let geo =
    {
      p = Vec.make_vec3 0.0 1.0 0.0;
      normal = Vec.make_vec3 0.0 1.0 0.0;
      front_face = true;
    }
  in

  match m.scatter incoming geo with
  | Some (scattered, attenuation) ->
      let expected_dir = Vec.reflect (Ray.direction incoming) geo.normal in
      assert_equal expected_dir (Vec.unit_vec (direction scattered))
  | None -> assert_failure "Should reflect"

(** [test_dielectric_refraction] tests dielectric material reflection and
    refraction *)
let test_dielectric_refraction _ =
  let m = dielectric 1.5 in
  let normal = Vec.make_vec3 0.0 1.0 0.0 in
  let geo = { p = Vec.zero; normal; front_face = true } in

  let grazing = Ray.make_ray Vec.zero (Vec.make_vec3 0.0 1.0 0.0) in
  match m.scatter grazing geo with
  | Some (scattered, _) ->
      let reflected = Vec.reflect (Ray.direction grazing) normal in
      assert_equal reflected (Ray.direction scattered)
  | None -> assert_failure "Should reflect"

(** [test_diffuse_light] confirms that light material diffuses correctly and
    emits light *)

let test_diffuse_light _ =
  let color = Vec.make_vec3 2.0 3.0 4.0 in
  let m = diffuse_light color in
  let geo = { p = Vec.zero; normal = Vec.zero; front_face = true } in

  assert_equal None (m.scatter (Ray.make_ray Vec.zero Vec.zero) geo);
  assert_equal color (m.emitted geo)

(** [test_uniform_diffuse] checks that material diffuses uniformly *)
let test_uniform_diffuse _ =
  let m = uniform_diffuse 0.5 in
  let geo =
    { p = Vec.zero; normal = Vec.make_vec3 0.0 1.0 0.0; front_face = true }
  in

  match m.scatter (Ray.make_ray Vec.zero Vec.zero) geo with
  | Some (scattered, attenuation) ->
      assert_bool "Hemisphere direction"
        (Vec.dot (direction scattered) geo.normal >= 0.0);
      assert_equal (Vec.smul 0.5 (Vec.make_vec3 1. 1. 1.)) attenuation
  | None -> assert_failure "Should scatter"

let suite =
  "Material Tests"
  >::: [
         "Empty material" >:: test_empty_material;
         "Lambertian scatter" >:: test_lambertian_scatter;
         "Metal reflection" >:: test_metal_reflection;
         "Dielectric behavior" >:: test_dielectric_refraction;
         "Diffuse light" >:: test_diffuse_light;
         "Uniform diffuse" >:: test_uniform_diffuse;
       ]

let () = run_test_tt_main suite
