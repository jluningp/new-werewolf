open Core
open Shared

type t

val create : unit -> t

val add_user : t -> Username.t -> unit Or_error.t

val page : t -> Page.t

val on_input : t -> Input.t -> unit Or_error.t
