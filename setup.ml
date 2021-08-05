open Core
open Shared

type t = { users : Username.Hash_set.t; mutable roles : (Role.t * int) list }

let create () = { users = Username.Hash_set.create (); roles = [] }

let add_user t user =
  if Hash_set.mem t.users user then Or_error.error_string "User already exists"
  else (
    Hash_set.add t.users user;
    Ok () )

let on_input t = function
  | Input.Ack | Input.Choose_user _ -> Or_error.error_string "Invalid input"
  | Choose_role { role; count } ->
      let roles =
        List.filter t.roles ~f:(fun (other_role, _) ->
            match (role, other_role) with
            | Werewolf, Werewolf | Robber, Robber -> false
            | _ -> true)
      in
      t.roles <- (role, count) :: roles;
      Ok ()

let page t =
  let roles_needed = Hash_set.length t.users + 3 in
  let roles_selected = List.fold t.roles ~init:0 ~f:(fun c (_, n) -> c + n) in
  [
    Page.Element.Text (sprintf "Choose %n roles" roles_needed);
    Choose_role [ (Werewolf, 5); (Robber, 1) ];
    Text (sprintf "%n roles needed" (roles_needed - roles_selected));
  ]
