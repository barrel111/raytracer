open Core
open Rendering

type t
(** A concrete type representing a camera. *)

val init_cam :
  float ->
  int ->
  int ->
  ?vfov:int ->
  ?look_from:Vec.t ->
  ?look_at:Vec.t ->
  ?vup:Vec.t ->
  ?bg:Color.t ->
  int ->
  t
(** [init_cam aspect_ratio img_width samples_per_px max_depth] initializes a
    camera with the corresponding properties. The optional parameters are:
    - [?vfov]: int, the vertical field-of-view in degrees. Default: [90].g.,
      20).
    - [?look_from]: Vec.t, the 3D vector representing the camera's position.
      Default: [Vec.make_vec3 0. 0. 0.].
    - [?look_at]: Vec.t, the 3D vector representing the point the camera is
      focused on. Default: [Vec.make_vec3 0. 0. (-1.)].
    - [?vup]: Vec.t, the camera's "view up" vector, defining its orientation.
      Default: [Vec.make_vec3 0. 1. 0.].
    - [?bg]: Color.t, the background color of the scene when rays hit nothing.
      Default: [{x = 1.; y = 1.; z = 1.;}]. *)

val render : t -> Hittable.t -> out_channel -> unit
(** [render cam obj out] renders [obj] to [out] using camera [cam]. *)

val img_width : t -> int
(** [img_width cam] returns the width of the image that [cam] renders. *)

val img_height : t -> int
(** [img_height cam] returns the height of the image that [cam] renders. *)
