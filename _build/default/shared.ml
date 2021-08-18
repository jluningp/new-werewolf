open Core
module Username = String
module Html = Html

module Role = struct
  type t =
    | Robber
    | Werewolf
    | Seer
    | Troublemaker
    | Villager
    | Insomniac
    | Mason
  [@@deriving sexp, equal, enumerate]

  let of_string str = t_of_sexp (Sexp.of_string str)

  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Page = struct
  module Element = struct
    type t =
      | Text of string
      | Cards of Role.t list
      | Choose_user of {
          choose_this_many : int;
          users : Username.t list;
          or_center : bool;
        }
      | Ack_button
      | Reveal_button
      | No_refresh

    let is_action = function
      | Text _ | Cards _ | Reveal_button -> false
      | Choose_user _ | Ack_button | No_refresh -> true

    let card_image role =
      match role with
      | Role.Werewolf -> "images/werewolves.png"
      | Robber -> "images/robbers.png"
      | Seer -> "images/seers.png"
      | Troublemaker -> "images/troublemakers.png"
      | Villager -> "images/villagers.png"
      | Insomniac -> "images/insomniacs.png"
      | Mason -> "images/masons.png"

    let to_html element =
      let open Html in
      match element with
      | Text str -> p [] [ text str ]
      | Cards [] -> p [] [ text "No Cards" ]
      | Cards [ card ] -> img [ ("src", card_image card) ] []
      | Cards cards ->
          let cards =
            List.map cards ~f:(fun card -> img [ ("src", card_image card) ] [])
          in
          div [] cards
      | Choose_user { choose_this_many; users; or_center } ->
          let users = if or_center then "center__cards__" :: users else users in
          let user_select =
            List.concat_map users ~f:(fun user ->
                let username =
                  match user with
                  | "center__cards__" -> "center cards"
                  | _ -> user
                in
                let typ =
                  if choose_this_many = 1 then "radio" else "checkbox"
                in
                [
                  input
                    [
                      ("type", typ);
                      ("name", "users");
                      ("id", "select" ^ user);
                      ("value", user);
                    ]
                    [];
                  label [ ("for", "select" ^ user) ] [ text username ];
                  br;
                ])
          in
          div []
            ( user_select
            @ [
                br;
                div
                  [ ("style", "text-align:center;") ]
                  [ button [ ("onclick", "chooseUsers()") ] [ text "GO" ] ];
              ] )
      | Ack_button ->
          div
            [ ("style", "text-align:center;") ]
            [ button [ ("onclick", "ack()") ] [ text "OK" ] ]
      | Reveal_button ->
          div
            [ ("style", "text-align:center;") ]
            [ button [ ("onclick", "reveal()") ] [ text "Reveal Cards" ] ]
      | No_refresh -> div [] []
  end

  type t = Element.t list

  let to_html t =
    let elements = List.map t ~f:Element.to_html in
    let refresh_page =
      if List.exists t ~f:Element.is_action then [] else [ Html.refresh_page ]
    in
    Html.div [] (elements @ refresh_page)
end

module Input = struct
  type t = Ack | Choose_user of Username.t list | View_center_cards | Reveal
  [@@deriving sexp]
end
