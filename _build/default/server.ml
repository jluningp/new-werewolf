open Core
open Async
open Cohttp_async
open Shared

type t = { mutable state : State.t }

module Query = struct
  module Page = State.Page
  module Action = State.Action

  type t =
    | Page of Username.t option
    | Action of { username : Username.t; action : Action.t }
    | File of string

  let available_files =
    [
      "script.js";
      "images/werewolves.png";
      "images/robbers.png";
      "images/seers.png";
      "images/troublemakers.png";
      "images/villagers.png";
      "images/insomniacs.png";
      "images/masons.png";
    ]
    |> String.Set.of_list

  let get_username uri = Uri.get_query_param uri "username"

  let parse uri =
    let open Option.Let_syntax in
    match Uri.path uri with
    | "/" -> Some (File "page.html")
    | "/page" -> Some (Page (get_username uri))
    | "/action/create" ->
        let%map username = get_username uri in
        Action { username; action = Create_game }
    | "/action/join" ->
        let%map username = get_username uri in
        Action { username; action = Join_game }
    | "/action/set_role" ->
        let%bind username = get_username uri in
        let%bind role =
          let%bind role = Uri.get_query_param uri "role" in
          Option.try_with (fun () -> Role.of_string role)
        in
        let%map count =
          let%bind count = Uri.get_query_param uri "count" in
          Option.try_with (fun () -> Int.of_string count)
        in
        Action { username; action = Set_role (role, count) }
    | "/action/input/ack" ->
        let%map username = get_username uri in
        Action { username; action = Game_input Ack }
    | "/action/input/choose_user" -> (
        let%bind username = get_username uri in
        let%map users = Uri.get_query_param' uri "users" in
        match users with
        | [ "center__cards__" ] ->
            Action { username; action = Game_input View_center_cards }
        | _ -> Action { username; action = Game_input (Choose_user users) } )
    | "/action/input/reveal" ->
        let%map username = get_username uri in
        Action { username; action = Game_input Reveal }
    | "/action/start_game" ->
        let%map username = get_username uri in
        Action { username; action = Start_game }
    | "/action/end_game" ->
        let%map username = get_username uri in
        Action { username; action = End_game }
    | filename ->
        let filename = String.drop_prefix filename 1 in
        if Set.mem available_files filename then Some (File filename) else None
end

let server port () =
  let t = { state = State.create () } in
  let callback ~body:_ _conn req =
    match Query.parse (Request.uri req) with
    | None -> Server.respond_string "Unknown request"
    | Some query -> (
        match query with
        | Action { action; username } ->
            State.action t.state action username;
            Server.respond_string ""
        | Page username ->
            let page = State.page t.state username in
            Server.respond_string ~status:`OK page
        | File file -> Server.respond_with_file file )
  in
  let%bind t =
    Server.create ~mode:`TCP ~on_handler_error:`Ignore
      (Async_unix.Tcp.Where_to_listen.of_port port)
      callback
  in
  printf "Serving at port %n" (Server.listening_on t);
  Deferred.never ()

let () =
  Command.run
    (Command.async ~summary:"The One Night Werewolf server"
       (Command.Spec.map
          (Command.Param.flag "port" ~doc:"port"
             (Command.Param.required Command.Param.int))
          ~f:server))