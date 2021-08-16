open Core
module Username = String
module Html = Html

module Role = struct
  type t = Robber | Werewolf [@@deriving sexp, equal, enumerate]

  let of_string = function
    | "Robber" -> Robber
    | "Werewolf" -> Werewolf
    | _ -> failwith "Unknown role"

  let to_string = function Robber -> "Robber" | Werewolf -> "Werewolf"
end

module Page = struct
  module Element = struct
    type t =
      | Text of string
      | Cards of Role.t list
      | Choose_user of { choose_this_many : int; users : Username.t list }
      | Ack_button

    let is_action = function
      | Text _ | Cards _ -> false
      | Choose_user _ | Ack_button -> true

    let to_html element =
      let open Html in
      match element with
      | Text str -> p [] [ text str ]
      | Cards [] -> p [] [ text "No Cards" ]
      | Cards [ card ] -> p [] [ text (Role.to_string card) ]
      | Cards cards ->
          let cards = List.map cards ~f:Role.to_string in
          let cards = List.map cards ~f:(fun card -> p [] [ text card ]) in
          ul [] cards
      | Choose_user { choose_this_many; users } ->
          let user_select =
            List.concat_map users ~f:(fun user ->
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
                  label [ ("for", "select" ^ user) ] [ text user ];
                  br;
                ])
          in
          div []
            ( user_select
            @ [ button [ ("onclick", "chooseUsers()") ] [ text "GO" ] ] )
      | Ack_button -> button [ ("onclick", "ack()") ] [ text "OK" ]
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
  type t = Ack | Choose_user of Username.t list [@@deriving sexp]
end
