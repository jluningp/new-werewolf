type t

val parse : Uri.t -> t Or_error.t

val to_uri : t -> Uri.t
