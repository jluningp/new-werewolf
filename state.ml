open Core
open Shared
module Game_code = String

module Page = struct
  type t = Join | Setup | Play of Username.t | Default
end

module Action = struct
  type t =
    | Create_game
    | Join_game of Game_code.t
    | New_game
    | Leave_game
    | Set_role of Role.t * int
    | Start_game
    | Game_input of Input.t
    | End_game
    | Set_page of [`Settings | `Setup]
    | Set_setting of {tag : string; is_on : bool}
end

module Join = struct
  let create_game =
    Html.div [("class", "button"); ("onclick", "createGame()")] [Html.text "GO"]

  let join_game =
    Html.div [("class", "button"); ("onclick", "joinGame()")] [Html.text "GO"]

  let get_page ~username =
    let username = Option.value ~default:"" username in
    let username_input =
      Html.(
        div []
          [ text "NAME: "
          ; input
              [("type", "text"); ("id", "username"); ("value", username)]
              []
          ; br ])
    in
    let game_code_input =
      Html.(
        input [("type", "text"); ("id", "code"); ("placeholder", "CODE")] [])
    in
    Html.div []
      [ username_input
      ; Html.br
      ; Html.text "New Game: "
      ; create_game
      ; Html.br
      ; Html.br
      ; Html.text "Join Game: "
      ; game_code_input
      ; Html.text " "
      ; join_game ]
    |> Html.to_string
end

module Settings = struct
  module Setting = struct
    type t = Lone_werewolf_sees_non_werewolf_center_card | Show_role_number
    [@@deriving enumerate, sexp]

    let to_tag t = sexp_of_t t |> Sexp.to_string
    let of_tag str = Option.try_with (fun () -> Sexp.of_string str |> t_of_sexp)
  end

  include Settings

  let create () =
    { lone_werewolf_sees_non_werewolf_center_card = false
    ; show_role_number = true }

  let set_setting t ~tag ~is_on =
    match Setting.of_tag tag with
    | None -> ()
    | Some setting -> (
      match setting with
      | Setting.Lone_werewolf_sees_non_werewolf_center_card ->
          t.lone_werewolf_sees_non_werewolf_center_card <- is_on
      | Show_role_number -> t.show_role_number <- is_on )

  let single_select t setting ~is_admin =
    let open Html in
    let checked =
      match setting with
      | Setting.Lone_werewolf_sees_non_werewolf_center_card ->
          t.lone_werewolf_sees_non_werewolf_center_card
      | Show_role_number -> t.show_role_number
    in
    let tag = Setting.to_tag setting in
    let name, description =
      match setting with
      | Setting.Lone_werewolf_sees_non_werewolf_center_card ->
          ( "Lone werewolf sees non-werewolf card"
          , "Rather than looking at one center card, the lone werewolf draws \
             center cards until they see one that is not a werewolf." )
      | Show_role_number ->
          ( "Show role number"
          , "If playing with multiple copies of a card where ordering matters \
             (i.e., 1st Robber, 2nd Robber), show the order number with the \
             card." )
    in
    div []
      [ label [("for", tag)] [text (name ^ ":  ")]
      ; label [("class", "switch")]
          [ input
              ( [ ("type", "checkbox")
                ; ("id", tag)
                ; ("onchange", "setSetting('" ^ tag ^ "')")
                ; ("type", "checkbox") ]
              @ (if is_admin then [] else [("disabled", "")])
              @ if checked then [("checked", "")] else [] )
              []
          ; span [("class", "slider round")] [] ]
      ; br
      ; label [("for", tag); ("class", "subtext")] [text description]
      ; br ]

  let get_page t ~is_admin =
    let open Html in
    div []
      ( [div [("style", "text-align:center")] [b [] [text "Settings"]]; br]
      @ List.map Setting.all ~f:(single_select t ~is_admin)
      @ [ div
            [("style", "text-align:center")]
            [ div
                [("id", "button"); ("onclick", "toSetup()")]
                [text "Back to Setup"] ] ] )
end

module Setup = struct
  type t =
    { mutable roles : (int * Role.t) list
    ; mutable users : Username.t list
    ; game_code : Game_code.t
    ; settings : Settings.t
    ; current_page : [`Settings | `Setup] Username.Table.t }

  let create ?settings ~roles ~game_code () =
    { roles
    ; users = []
    ; game_code
    ; settings =
        ( match settings with
        | None -> Settings.create ()
        | Some settings -> settings )
    ; current_page = Username.Table.create () }

  let number_select t role ~is_admin =
    let open Html in
    let roles =
      List.filter_map t.roles ~f:(fun (n, r) ->
          match (role, r) with
          | Role.Robber _, Robber _ -> Some n
          | Troublemaker _, Troublemaker _ -> Some n
          | Drunk _, Drunk _ -> Some n
          | _, _ -> if Role.equal role r then Some n else None )
      |> List.fold ~init:0 ~f:( + ) |> Int.to_string
    in
    let role_str = Role.to_string_unnumbered role in
    let input =
      input
        [ ("name", "quantity")
        ; ("type", "number")
        ; ("class", "quantity__input")
        ; ("value", roles)
        ; ("id", role_str)
        ; ("onchange", "changeNumberedRole('" ^ role_str ^ "')") ]
        []
    in
    div []
      ( if is_admin then
        [ div [("class", "quantity")]
            [ a
                [ ("class", "quantity__minus")
                ; ("onclick", "decrInput('" ^ role_str ^ "')") ]
                [span [("class", "prevent_select")] [text "-"]]
            ; input
            ; a
                [ ("class", "quantity__plus")
                ; ("onclick", "incrInput('" ^ role_str ^ "')") ]
                [span [("class", "prevent_select")] [text "+"]]
            ; label
                [("for", role_str); ("class", "quantity_label")]
                [text ("&nbsp;&nbsp;" ^ role_str)] ] ]
      else
        [ div [("class", "quantity")]
            [ a [("class", "quantity__input")] [text roles]
            ; label
                [("for", role_str); ("class", "quantity_label")]
                [text ("&nbsp;&nbsp;" ^ role_str)] ] ] )

  let role_input t role ~is_admin = number_select t role ~is_admin

  let get_page t user =
    let html =
      let open Html in
      let is_admin =
        match List.last t.users with
        | None -> false
        | Some u -> Username.equal u user
      in
      let page =
        match Hashtbl.find t.current_page user with
        | None ->
            Hashtbl.set t.current_page ~key:user ~data:`Setup ;
            `Setup
        | Some page -> page
      in
      match page with
      | `Settings -> Settings.get_page t.settings ~is_admin
      | `Setup ->
          let inputs = List.map Role.all ~f:(role_input t ~is_admin) in
          let players = String.concat ~sep:", " t.users in
          let refresh = if is_admin then " (&#8635; page to update)" else "" in
          div []
            [ div
                [("style", "text-align:right")]
                [ div
                    [ ("id", "gear")
                    ; ("onclick", "toSettings()")
                    ; ("size", "2em") ]
                    [text "&#9881;"] ]
            ; div
                [("style", "text-align:center")]
                [b [] [text ("New Game: " ^ t.game_code)]]
            ; br
            ; ( if is_admin then text "Select 3 more roles than players:"
              else text "Selecting roles:" )
            ; br
            ; br
            ; div [("style", "padding-left:0.3em")] inputs
            ; div
                [("style", "font-size:.7em; color:gray;")]
                [br; text ("Players" ^ refresh ^ ": " ^ players)]
            ; ( if is_admin then
                div
                  [("style", "text-align:center")]
                  [ div
                      [("id", "button"); ("onclick", "startGame()")]
                      [text "Start Game"] ]
              else refresh_page ) ]
    in
    Html.to_string html

  let add_user t user =
    if List.mem t.users user ~equal:Username.equal then
      Or_error.error_string "Duplicate username"
    else (
      t.users <- user :: t.users ;
      Hashtbl.set t.current_page ~key:user ~data:`Setup ;
      Ok () )

  let remove_user t user =
    t.users
    <- List.filter t.users ~f:(fun username ->
           not (Username.equal user username) ) ;
    Ok ()

  let set_page t user page = Hashtbl.set t.current_page ~key:user ~data:page
  let set_setting t ~tag ~is_on = Settings.set_setting t.settings ~tag ~is_on

  let set_numbered_role t role count =
    if count >= 0 then
      let roles =
        List.filter t.roles ~f:(fun (_, other_role) ->
            match (role, other_role) with
            | Role.Troublemaker _, Troublemaker _ -> false
            | Robber _, Robber _ -> false
            | Drunk _, Drunk _ -> false
            | _ -> true )
      in
      let numbered_role =
        List.init count ~f:(fun i ->
            match role with
            | Role.Troublemaker _ -> (1, Role.Troublemaker i)
            | Robber _ -> (1, Robber i)
            | Drunk _ -> (1, Drunk i)
            | _ ->
                failwith
                  "Tried to set numbered role, but role is not robber, drunk, \
                   or troublemaker" )
      in
      if count > 0 then t.roles <- numbered_role @ roles else t.roles <- roles

  let set_unnumbered_role t role count =
    if count >= 0 then
      let roles =
        List.filter t.roles ~f:(fun (_, other_role) ->
            not (Role.equal role other_role) )
      in
      if count > 0 then t.roles <- (count, role) :: roles else t.roles <- roles

  let set_role t role count =
    match role with
    | Role.Troublemaker _ | Role.Robber _ | Role.Drunk _ ->
        set_numbered_role t role count
    | _ -> set_unnumbered_role t role count
end

type status = Setup of Setup.t | Play of Werewolf.t

module Game_state = struct
  type t =
    { mutable status : status
    ; mutable last_game_roles : (int * Role.t) list
    ; mutable last_game_settings : Settings.t
    ; mutable users : Username.t list }

  let create ~creator ~code =
    let setup = Setup.create ~roles:[] ~game_code:code () in
    let _ : unit Or_error.t = Setup.add_user setup creator in
    let users = [creator] in
    { status = Setup setup
    ; last_game_roles = []
    ; last_game_settings = setup.settings
    ; users }
end

type t =
  {games : Game_state.t Game_code.Table.t; users : Game_code.t Username.Table.t}

let create () =
  {games = Game_code.Table.create (); users = Username.Table.create ()}

let get_game t username =
  let open Option.Let_syntax in
  let%bind username = username in
  let%bind game_code = Hashtbl.find t.users username in
  Hashtbl.find t.games game_code

let rec new_code t =
  let code =
    String.of_char_list
      (List.init 4 ~f:(fun _ -> Char.of_int_exn (65 + Random.int 26)))
  in
  if Hashtbl.mem t.games code then new_code t else code

let page t username =
  match (get_game t username, username) with
  | None, _ | _, None -> Join.get_page ~username
  | Some game, Some username -> (
    match game.status with
    | Setup setup -> Setup.get_page setup username
    | Play werewolf ->
        Html.to_string
          (Shared.Page.to_html (Werewolf.get_page werewolf username)) )

let action t action username =
  match get_game t (Some username) with
  | None -> (
    match action with
    | Action.Create_game ->
        let game_code = new_code t in
        let game = Game_state.create ~creator:username ~code:game_code in
        Hashtbl.set t.games ~key:game_code ~data:game ;
        Hashtbl.set t.users ~key:username ~data:game_code
    | Join_game code -> (
      match Hashtbl.find t.games code with
      | None -> ()
      | Some game -> (
        match game.status with
        | Play _ -> ()
        | Setup setup ->
            let _ : unit Or_error.t = Setup.add_user setup username in
            () ;
            game.users <- username :: game.users ;
            Hashtbl.set t.users ~key:username ~data:code ) )
    | Leave_game | Set_role _ | Start_game | Game_input _ | End_game
     |New_game | Set_page _ | Set_setting _ ->
        () )
  | Some game -> (
    match (game.status, action) with
    | Setup setup, Leave_game ->
        let game_code = Hashtbl.find_exn t.users username in
        let _ : unit Or_error.t = Setup.remove_user setup username in
        game.users
        <- List.filter game.users ~f:(fun user ->
               not (String.equal user username) ) ;
        Hashtbl.remove t.users username ;
        if List.is_empty game.users then Hashtbl.remove t.games game_code
    | Setup setup, Set_role (role, count) -> Setup.set_role setup role count
    | Setup setup, Start_game -> (
        game.last_game_roles <- setup.roles ;
        game.last_game_settings <- setup.settings ;
        let roles =
          List.concat_map setup.roles ~f:(fun (n, role) ->
              List.init n ~f:(fun _ -> role) )
        in
        match Werewolf.create setup.settings roles setup.users with
        | Error _ -> ()
        | Ok werewolf -> game.status <- Play werewolf )
    | Setup setup, Set_page page -> Setup.set_page setup username page
    | Setup setup, Set_setting {tag; is_on} ->
        Setup.set_setting setup ~tag ~is_on
    | Play werewolf, Game_input input ->
        let _ : unit Or_error.t = Werewolf.on_input werewolf username input in
        ()
    | _, End_game ->
        let game_code = Hashtbl.find_exn t.users username in
        List.iter game.users ~f:(Hashtbl.remove t.users) ;
        Hashtbl.remove t.games game_code
    | _, New_game ->
        let game_code = Hashtbl.find_exn t.users username in
        let setup =
          Setup.create ~roles:game.last_game_roles
            ~settings:(Settings.copy game.last_game_settings)
            ~game_code ()
        in
        List.iter (List.rev game.users) ~f:(fun user ->
            ignore (Setup.add_user setup user : unit Or_error.t) ) ;
        game.status <- Setup setup
    | _ -> () )
