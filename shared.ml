open Core
module Username = String
module Html = Html

module Role = struct
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
    | Doppelganger of t option
  [@@deriving sexp, equal]

  let all =
    [
      Robber 0;
      Werewolf;
      Seer;
      Troublemaker 0;
      Villager;
      Insomniac;
      Mason;
      Tanner;
      Minion;
      Drunk 0;
      Hunter;
      Mystic_wolf;
      Dream_wolf;
      Alpha_wolf;
      Doppelganger None;
    ]

  let card_image role =
    match role with
    | Werewolf -> "images/werewolves.png"
    | Robber _ -> "images/robbers.png"
    | Seer -> "images/seers.png"
    | Troublemaker _ -> "images/troublemakers.png"
    | Villager -> "images/villagers.png"
    | Insomniac -> "images/insomniacs.png"
    | Mason -> "images/masons.png"
    | Minion -> "images/minions.png"
    | Drunk _ -> "images/drunks.png"
    | Tanner -> "images/tanners.png"
    | Hunter -> "images/hunter.jpeg"
    | Mystic_wolf -> "images/mystic_wolf.png"
    | Dream_wolf -> "images/dream_wolf.png"
    | Alpha_wolf -> "images/alpha_wolf.png"
    | Doppelganger _ -> "images/doppelgangers.png"

  let rec cardinal_to_string = function
    | 0 -> "0th"
    | 1 -> "1st"
    | 2 -> "2nd"
    | 3 -> "3rd"
    | n when n <= 20 -> Int.to_string n ^ "th"
    | n ->
        let last_digit = n % 10 in
        let digits = n / 10 in
        Int.to_string digits ^ cardinal_to_string last_digit

  let cardinal_of_string str =
    String.prefix str (String.length str - 2) |> Int.of_string

  let of_string str =
    match str with
    | "Doppelganger" -> Doppelganger None
    | "Dream Wolf" -> Dream_wolf
    | "Mystic Wolf" -> Mystic_wolf
    | "Alpha Wolf" -> Alpha_wolf
    | _ -> (
        let numbered_role =
          let numbered_roles =
            [
              ((fun n -> Robber n), " Robber");
              ((fun n -> Troublemaker n), " Troublemaker");
              ((fun n -> Drunk n), " Drunk");
            ]
          in
          List.find_map numbered_roles ~f:(fun (make_role, suffix) ->
              match str with
              | "Troublemaker" -> Some (Troublemaker 0)
              | "Drunk" -> Some (Drunk 0)
              | "Robber" -> Some (Robber 0)
              | _ ->
                  if String.is_suffix str ~suffix then
                    Some (make_role (cardinal_of_string (String.prefix str 3)))
                  else None)
        in
        match numbered_role with
        | None -> t_of_sexp (Sexp.of_string str)
        | Some role -> role)

  let to_string t =
    match t with
    | Doppelganger _ -> "Doppelganger"
    | Dream_wolf -> "Dream Wolf"
    | Mystic_wolf -> "Mystic Wolf"
    | Alpha_wolf -> "Alpha Wolf"
    | Drunk n -> cardinal_to_string (n + 1) ^ " Drunk"
    | Troublemaker n -> cardinal_to_string (n + 1) ^ " Troublemaker"
    | Robber n -> cardinal_to_string (n + 1) ^ " Robber"
    | _ -> Sexp.to_string (sexp_of_t t)

  let to_string_unnumbered t =
    match t with
    | Doppelganger _ -> "Doppelganger"
    | Dream_wolf -> "Dream Wolf"
    | Mystic_wolf -> "Mystic Wolf"
    | Alpha_wolf -> "Alpha Wolf"
    | Drunk _ -> "Drunk"
    | Troublemaker _ -> "Troublemaker"
    | Robber _ -> "Robber"
    | _ -> Sexp.to_string (sexp_of_t t)
end

module Page = struct
  module Element = struct
    type t =
      | Text of string
      | Html of string
      | Centered_text of string
      | Cards of Role.t list
      | Choose_user of {
          choose_this_many : int;
          users : Username.t list;
          or_center : bool;
          or_no_werewolf : bool;
        }
      | Ack_button
      | Vote_button
      | No_refresh

    let is_action = function
      | Text _ | Html _ | Centered_text _ | Cards _ -> false
      | Choose_user _ | Ack_button | Vote_button | No_refresh -> true

    let to_html element =
      let open Html in
      match element with
      | Text str -> p [] [ text str ]
      | Html str -> text str
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
      | Choose_user { choose_this_many; users; or_center; or_no_werewolf } ->
          let users = if or_center then "center__cards__" :: users else users in
          let users =
            if or_no_werewolf then "no__werewolf__" :: users else users
          in
          let user_select =
            List.map users ~f:(fun user ->
                let username =
                  match user with
                  | "center__cards__" -> "center cards"
                  | "no__werewolf__" -> "no werewolf"
                  | _ -> user
                in
                let typ =
                  if choose_this_many = 1 then "radio" else "checkbox"
                in
                [
                  label
                    [ ("class", "switch") ]
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
            (user_select
            @ [
                div
                  [ ("style", "text-align:center;") ]
                  [
                    div
                      [ ("id", "button"); ("onclick", "chooseUsers()") ]
                      [ text "GO" ];
                  ];
              ])
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
      | No_refresh -> text ""
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
  type t =
    | Ack
    | Choose_user of Username.t list
    | View_center_cards
    | Choose_no_werewolf
    | Vote
  [@@deriving sexp]
end
