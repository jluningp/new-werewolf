open Core
module Username = String

module Role : sig
  type t = Robber | Werewolf
end

module Page : sig
  module Element : sig
    type t =
      | Text of string
      | Cards of Role.t list
      | Choose_user of { choose_this_many : int; users : Username.t list }
  end

  type t = Element.t list
end

module Input : sig
  type t = Ack | Choose_user of Username.t list
end

type t

val create : Role.t list -> Username.t list -> t Or_error.t

val add_input : t -> Username.t -> Input.t -> unit Or_error.t

val get_page : t -> Username.t -> Page.t
