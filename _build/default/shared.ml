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
    | Tanner
    | Minion
    | Drunk
    | Doppelganger of t option
  [@@deriving sexp, equal]

  let all =
    [
      Robber;
      Werewolf;
      Seer;
      Troublemaker;
      Villager;
      Insomniac;
      Mason;
      Tanner;
      Minion;
      Drunk;
      Doppelganger None;
    ]

  let card_image role =
    match role with
    | Werewolf -> "images/werewolves.png"
    | Robber -> "images/robbers.png"
    | Seer -> "images/seers.png"
    | Troublemaker -> "images/troublemakers.png"
    | Villager -> "images/villagers.png"
    | Insomniac -> "images/insomniacs.png"
    | Mason -> "images/masons.png"
    | Minion -> "images/minions.png"
    | Drunk -> "images/drunks.png"
    | Tanner -> "images/tanners.png"
    | Doppelganger _ -> "images/doppelgangers.png"

  let of_string str =
    match str with
    | "Doppelganger" -> Doppelganger None
    | _ -> t_of_sexp (Sexp.of_string str)

  let to_string t =
    match t with
    | Doppelganger _ -> "Doppelganger"
    | _ -> Sexp.to_string (sexp_of_t t)
end

module Page = struct
  module Element = struct
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

    let is_action = function
      | Text _ | Centered_text _ | Cards _ -> false
      | Choose_user _ | Ack_button | Vote_button | No_refresh -> true

    let to_html element =
      let open Html in
      match element with
      | Text str -> p [] [ text str ]
      | Centered_text str ->
          div [ ("style", "text-align:center;") ] [ text str ]
      | Cards [] -> p [] [ text "No Cards" ]
      | Cards [ card ] ->
          div
            [ ("style", "text-align:center;") ]
            [ img [ ("src", Role.card_image card) ] [] ]
      | Cards cards ->
          let cards =
            List.map cards ~f:(fun card ->
                img [ ("src", Role.card_image card) ] [])
          in
          div [ ("style", "text-align:center;") ] [ div [] cards ]
      | Choose_user { choose_this_many; users; or_center } ->
          let users = if or_center then "center__cards__" :: users else users in
          let user_select =
            List.map users ~f:(fun user ->
                let username =
                  match user with
                  | "center__cards__" -> "center cards"
                  | _ -> user
                in
                let typ =
                  if choose_this_many = 1 then "radio" else "checkbox"
                in
                [
                  label [ ("class", "switch") ]
                    [
                      input
                        [
                          ("type", typ);
                          ("name", "users");
                          ("id", "select" ^ user);
                          ("value", user);
                        ]
                        [];
                      span [ ("class", "slider round") ] [];
                    ];
                  label [ ("for", "select" ^ user) ] [ text ("  " ^ username) ];
                ])
            |> List.intersperse ~sep:[ br; br ]
            |> List.concat
          in
          div []
            ( user_select
            @ [
                div
                  [ ("style", "text-align:center;") ]
                  [
                    div
                      [ ("id", "button"); ("onclick", "chooseUsers()") ]
                      [ text "GO" ];
                  ];
              ] )
      | Ack_button ->
          div
            [ ("style", "text-align:center;") ]
            [ div [ ("id", "button"); ("onclick", "ack()") ] [ text "OK" ] ]
      | Vote_button ->
          div
            [ ("style", "text-align:center;") ]
            [
              div
                [ ("id", "button"); ("onclick", "vote()") ]
                [ text "Ready to Vote" ];
            ]
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
  type t = Ack | Choose_user of Username.t list | View_center_cards | Vote
  [@@deriving sexp]
end
