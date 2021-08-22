open Core
module Username = String
module Html = Html

module Role : sig
  type t =
    | Robber
    | Werewolf
    | Seer
    | Troublemaker
    | Villager
    | Insomniac
    | Mason
    | Tanner
    | Minion
    | Drunk
    | Doppelganger of t option
  [@@deriving sexp, equal, enumerate]

  val card_image : t -> string

  val to_string : t -> string

  val of_string : string -> t
end

module Page : sig
  module Element : sig
    type t =
      | Text of string
      | Centered_text of string
      | Cards of Role.t list
      | Choose_user of {
          choose_this_many : int;
          users : Username.t list;
          or_center : bool;
        }
      | Ack_button
      | Vote_button
      | No_refresh
  end

  type t = Element.t list

  val to_html : t -> Html.t
end

module Input : sig
  type t = Ack | Choose_user of Username.t list | View_center_cards | Vote
  [@@deriving sexp]
end
