open Core

type t = Vec.t
(** A color type, alias for vec3 *)

val write_color : out_channel -> t -> unit
(** [write_color out pixel_color] writes the color components of [pixel_color]
    to the output stream [out] *)
