open Core
module Username = String
module Html = Html

module Role : sig
  type t =
    | Robber of int
    | Werewolf
    | Seer
    | Troublemaker of int
    | Villager
    | Insomniac
    | Mason
    | Tanner
    | Minion
    | Drunk of int
    | Hunter
    | Mystic_wolf
    | Dream_wolf
    | Alpha_wolf
    | Voodoo_lou of int
    | Doppelganger of t option
  [@@deriving sexp, compare, enumerate]

  val equal : t -> t -> bool
  val card_image : t -> string
  val to_string : t -> string
  val to_string_unnumbered : t -> string
  val of_string : string -> t
end

module Page : sig
  module Element : sig
    type t =
      | Text of string
      | Html of string
      | Centered_text of string
      | Cards of Role.t list
      | Choose_user of
          { choose_this_many : int
          ; users : Username.t list
          ; or_center : bool
          ; or_no_werewolf : bool }
      | Ack_button
      | Vote_button
      | No_refresh
  end

  type t = Element.t list

  val to_html : t -> Html.t
end

module Input : sig
  type t =
    | Ack
    | Choose_user of Username.t list
    | View_center_cards
    | Choose_no_werewolf
    | Vote
  [@@deriving sexp]
end
