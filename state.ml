open Core
open Shared

module Page = struct
  type t = Join | Setup | Play of Username.t | Default
end

module Action = struct
  type t =
    | Create_game
    | Join_game
    | Set_role of Role.t * int
    | Start_game
    | Game_input of Input.t
    | End_game
end

module Join = struct
  let create_game =
    Html.div
      [ ("style", "text-align:center;") ]
      [
        Html.button [ ("onclick", "createGame()") ] [ Html.text "Create Game" ];
      ]

  let join_game =
    Html.div
      [ ("style", "text-align:center;") ]
      [ Html.button [ ("onclick", "joinGame()") ] [ Html.text "Join Game" ] ]

  let get_page ~existing_game ~username =
    let username = Option.value ~default:"" username in
    let username_input =
      Html.(
        div []
          [
            text "Username: ";
            input
              [ ("type", "text"); ("id", "username"); ("value", username) ]
              [];
          ])
    in
    Html.div []
      [ username_input; (if existing_game then join_game else create_game) ]
    |> Html.to_string
end

module Setup = struct
  type t = {
    mutable roles : (int * Role.t) list;
    mutable users : Username.t list;
  }

  let create () = { roles = []; users = [] }

  let role_input t role ~is_admin =
    let open Html in
    match role with
    | Role.Werewolf ->
        let werewolves =
          List.find_map t.roles ~f:(fun (n, role) ->
              if Role.equal Werewolf role then Some (Int.to_string n) else None)
          |> Option.value ~default:"0"
        in
        div []
          [
            label [ ("for", "werewolves") ] [ text "Werewolves: " ];
            ( if is_admin then
              input
                [
                  ("type", "number");
                  ("id", "werewolves");
                  ("value", werewolves);
                  ("onchange", "changeWerewolf()");
                ]
                []
            else text werewolves );
          ]
    | Robber ->
        let robber =
          List.find t.roles ~f:(fun (n, role) ->
              Role.equal Robber role && n > 0)
        in
        let checked = Option.is_some robber in
        div []
          [
            label [ ("for", "robber") ] [ text "Robber:  " ];
            ( if is_admin then
              input
                ( [
                    ("id", "robber");
                    ("onchange", "changeRobber()");
                    ("type", "checkbox");
                  ]
                @ if checked then [ ("checked", "") ] else [] )
                []
            else text (if checked then "[x]" else "[ ]") );
          ]

  let get_page t user =
    let html =
      let open Html in
      let is_admin =
        match List.last t.users with
        | None -> false
        | Some u -> Username.equal u user
      in
      let inputs = List.map Role.all ~f:(role_input t ~is_admin) in
      let players = String.concat ~sep:", " t.users in
      div []
        [
          div [ ("style", "text-align:center") ] [ b [] [ text "New Game" ] ];
          br;
          br;
          text
            ( "Players: " ^ players
            ^ if is_admin then "           (refresh to see new players)" else ""
            );
          br;
          br;
          ( if is_admin then text "Select 3 more roles than players:"
          else text "Selecting roles:" );
          br;
          div [ ("style", "padding-left:0.3em") ] inputs;
          br;
          ( if is_admin then
            div
              [ ("style", "text-align:center") ]
              [ button [ ("onclick", "startGame()") ] [ text "Start Game" ] ]
          else refresh_page );
        ]
    in
    Html.to_string html

  let add_user t user =
    if List.mem t.users user ~equal:Username.equal then
      Or_error.error_string "Duplicate username"
    else (
      t.users <- user :: t.users;
      Ok () )

  let set_role t role count =
    if count >= 0 then
      let roles =
        List.filter t.roles ~f:(fun (_, other_role) ->
            not (Role.equal role other_role))
      in
      if count > 0 then t.roles <- (count, role) :: roles else t.roles <- roles
end

type status = No_game | Setup of Setup.t | Play of Werewolf.t

type t = { mutable status : status }

let create () = { status = No_game }

let page t username =
  match (t.status, username) with
  | No_game, username -> Join.get_page ~existing_game:false ~username
  | Setup _, None -> Join.get_page ~existing_game:true ~username:None
  | Setup setup, Some username ->
      if List.mem setup.users username ~equal:Username.equal then
        Setup.get_page setup username
      else Join.get_page ~existing_game:true ~username:(Some username)
  | Play werewolf, Some username ->
      Html.to_string (Shared.Page.to_html (Werewolf.get_page werewolf username))
  | Play _, None -> ""

let action t action username =
  match (t.status, action) with
  | No_game, Action.Create_game ->
      let setup = Setup.create () in
      let (_ : unit Or_error.t) = Setup.add_user setup username in
      t.status <- Setup setup
  | Setup _, Create_game -> ()
  | Setup setup, Join_game ->
      let (_ : unit Or_error.t) = Setup.add_user setup username in
      ()
  | Setup setup, Set_role (role, count) -> Setup.set_role setup role count
  | Setup setup, Start_game -> (
      let roles =
        List.concat_map setup.roles ~f:(fun (n, role) ->
            List.init n ~f:(fun _ -> role))
      in
      match Werewolf.create roles setup.users with
      | Error _ -> ()
      | Ok werewolf -> t.status <- Play werewolf )
  | Play werewolf, Game_input input ->
      let (_ : unit Or_error.t) = Werewolf.on_input werewolf username input in
      ()
  | _, End_game -> t.status <- No_game
  | _ -> ()