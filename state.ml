open Core
open Shared

module Page = struct
  type t = Join | Setup | Play of Username.t | Default
end

module Action = struct
  type t =
    | Create_game
    | Join_game
    | Leave_game
    | Set_role of Role.t * int
    | Start_game
    | Game_input of Input.t
    | End_game
end

module Join = struct
  let create_game =
    Html.div
      [ ("id", "button"); ("onclick", "createGame()") ]
      [ Html.text "GO" ]

  let join_game =
    Html.div [ ("id", "button"); ("onclick", "joinGame()") ] [ Html.text "GO" ]

  let get_page ~existing_game ~username =
    let username = Option.value ~default:"" username in
    let username_input =
      Html.(
        div []
          [
            text "NAME: ";
            input
              [ ("type", "text"); ("id", "username"); ("value", username) ]
              [];
          ])
    in
    Html.div []
      [
        username_input;
        Html.br;
        Html.div
          [ ("style", "font-size:small;") ]
          [
            Html.text
              "Press \"GO\" to join the game. If no game currently exists, one \
               will be created.";
          ];
        (if existing_game then join_game else create_game);
      ]
    |> Html.to_string
end

module Setup = struct
  type t = {
    mutable roles : (int * Role.t) list;
    mutable users : Username.t list;
  }

  let create ~roles = { roles; users = [] }

  let number_select t role ~is_admin =
    let open Html in
    let roles =
      List.find_map t.roles ~f:(fun (n, r) ->
          if Role.equal role r then Some (Int.to_string n) else None)
      |> Option.value ~default:"0"
    in
    let role_str = Role.to_string role in
    div []
      [
        label [ ("for", role_str) ] [ text (role_str ^ ": ") ];
        input
          ( [
              ("type", "number");
              ("id", role_str);
              ("value", roles);
              ("onchange", "changeNumberedRole('" ^ role_str ^ "')");
            ]
          @ if is_admin then [] else [ ("disabled", "") ] )
          [];
      ]

  let single_select t role ~is_admin =
    let open Html in
    let roles =
      List.find t.roles ~f:(fun (n, r) -> Role.equal role r && n > 0)
    in
    let role_str = Role.to_string role in
    let checked = Option.is_some roles in
    div []
      [
        label [ ("for", role_str) ] [ text (role_str ^ ":  ") ];
        label [ ("class", "switch") ]
          [
            input
              ( [
                  ("type", "checkbox");
                  ("id", role_str);
                  ("onchange", "changeSingleRole('" ^ role_str ^ "')");
                  ("type", "checkbox");
                ]
              @ (if is_admin then [] else [ ("disabled", "") ])
              @ if checked then [ ("checked", "") ] else [] )
              [];
            span [ ("class", "slider round") ] [];
          ];
      ]

  let role_input t role ~is_admin =
    match role with
    | Role.Werewolf | Villager | Insomniac | Mason | Seer | Tanner | Minion ->
        number_select t role ~is_admin
    | Robber | Troublemaker | Drunk | Doppleganger _ ->
        single_select t role ~is_admin

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
      let refresh = if is_admin then " (&#8635; page to update)" else "" in
      div []
        [
          div [ ("style", "text-align:center") ] [ b [] [ text "New Game" ] ];
          br;
          ( if is_admin then text "Select 3 more roles than players:"
          else text "Selecting roles:" );
          br;
          div [ ("style", "padding-left:0.3em") ] inputs;
          div
            [ ("style", "font-size:.7em; color:gray;") ]
            [ br; text ("Players" ^ refresh ^ ": " ^ players) ];
          ( if is_admin then
            div
              [ ("style", "text-align:center") ]
              [
                div
                  [ ("id", "button"); ("onclick", "startGame()") ]
                  [ text "Start Game" ];
              ]
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

  let remove_user t user =
    t.users <-
      List.filter t.users ~f:(fun username ->
          not (Username.equal user username));
    Ok ()

  let set_role t role count =
    if count >= 0 then
      let roles =
        List.filter t.roles ~f:(fun (_, other_role) ->
            not (Role.equal role other_role))
      in
      if count > 0 then t.roles <- (count, role) :: roles else t.roles <- roles
end

type status = No_game | Setup of Setup.t | Play of Werewolf.t

type t = {
  mutable status : status;
  mutable last_game_roles : (int * Role.t) list;
}

let create () = { status = No_game; last_game_roles = [] }

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
      let setup = Setup.create ~roles:t.last_game_roles in
      let (_ : unit Or_error.t) = Setup.add_user setup username in
      t.status <- Setup setup
  | Setup _, Create_game -> ()
  | Setup setup, Join_game ->
      let (_ : unit Or_error.t) = Setup.add_user setup username in
      ()
  | Setup setup, Leave_game ->
      let (_ : unit Or_error.t) = Setup.remove_user setup username in
      ()
  | Setup setup, Set_role (role, count) -> Setup.set_role setup role count
  | Setup setup, Start_game -> (
      t.last_game_roles <- setup.roles;
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
