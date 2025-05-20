open Core

type t = {
  scatter : Ray.t -> Geometry.t -> (Ray.t * Color.t) option;
  emitted : Geometry.t -> Color.t;
}
(** A type describing both how rays scatter and what color is emitted. *)

val empty : unit -> t
(** A material that neither scatters nor emits. *)

val lambertian : Color.t -> t
(** Diffuse (Lambertian) reflector with given albedo. *)

val metal : Color.t -> float -> t
(** Metallic reflector with given albedo and fuzziness. *)

val uniform_diffuse : float -> t
(** Hemispherical (uniform) diffuser with given attenuation factor. *)

val dielectric : float -> t
(** Dielectric (glass) material with given refractive index. *)

val diffuse_light : Color.t -> t
(** Emissive material that never scatters and always emits the given color. *)
