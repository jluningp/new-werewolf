open Core
open Shared

type t

val create : Role.t list -> Username.t list -> t Or_error.t
val on_input : t -> Username.t -> Input.t -> unit Or_error.t
val get_page : t -> Username.t -> Page.t
