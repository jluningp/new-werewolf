module Attr : sig
  type t = string * string
end

type t

val div : Attr.t list -> t list -> t

val button : Attr.t list -> t list -> t

val p : Attr.t list -> t list -> t

val label : Attr.t list -> t list -> t

val li : Attr.t list -> t list -> t

val ul : Attr.t list -> t list -> t

val input : Attr.t list -> t list -> t

val text : string -> t

val br : t

val refresh_page : t

val b : Attr.t list -> t list -> t

val img : Attr.t list -> t list -> t

val to_string : t -> string
