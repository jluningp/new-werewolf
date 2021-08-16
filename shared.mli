open Core
module Username = String
module Html = Html

module Role : sig
  type t = Robber | Werewolf [@@deriving sexp, equal, enumerate]

  val of_string : string -> t
end

module Page : sig
  module Element : sig
    type t =
      | Text of string
      | Cards of Role.t list
      | Choose_user of { choose_this_many : int; users : Username.t list }
      | Ack_button
  end

  type t = Element.t list

  val to_html : t -> Html.t
end

module Input : sig
  type t = Ack | Choose_user of Username.t list [@@deriving sexp]
end
