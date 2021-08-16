open Core
open Shared

module Random_cache = struct
  type t = { seer_doesnt_see_card : int; werewolf_sees_card : int }
end

module Phase = struct
  module Night = struct
    type t = Viewer | Robber | Troublemaker | Insomniac
  end

  type t = View_roles | Day | Night of Night.t | Vote | Results

  let _all =
    [
      View_roles;
      Day;
      Vote;
      Results;
      Night Viewer;
      Night Robber;
      Night Troublemaker;
      Night Insomniac;
    ]
end

module User = struct
  type t = {
    username : Username.t;
    mutable inputs : Input.t list;
    original_role : Role.t;
    mutable current_role : Role.t;
  }

  let create username role =
    { username; inputs = []; original_role = role; current_role = role }
end

type t = {
  users : User.t Username.Table.t;
  random_cache : Random_cache.t;
  center_cards : Role.t list;
  mutable phase : Phase.t;
}

let center_card_username = "CENTER-CARD-231804952714"

let center_card_users =
  [ center_card_username; center_card_username; center_card_username ]

let assign_roles roles users =
  let users = List.permute (center_card_users @ users) in
  match List.zip users roles with
  | Unequal_lengths -> Or_error.error_string "Incorrect number of roles."
  | Ok role_assignments ->
      let center_cards, user_roles =
        List.partition_map role_assignments ~f:(fun (user, role) ->
            if String.equal user center_card_username then First role
            else Second (user, role))
      in
      Ok (center_cards, user_roles)

let create roles users =
  match assign_roles roles users with
  | Error err -> Error err
  | Ok (center_cards, user_roles) -> (
      match Username.Table.of_alist user_roles with
      | `Duplicate_key _ -> Or_error.error_string "Duplicate username"
      | `Ok users ->
          Ok
            {
              users =
                Hashtbl.mapi users ~f:(fun ~key ~data -> User.create key data);
              random_cache =
                {
                  seer_doesnt_see_card = Random.int 3;
                  werewolf_sees_card = Random.int 3;
                };
              phase = View_roles;
              center_cards;
            } )

let is_users_turn night_phase (user : User.t) =
  match (night_phase, user.original_role) with
  | Phase.Night.Viewer, Werewolf -> true
  | Robber, Robber -> true
  | _, _ -> false

let get_page_for_robber t (user : User.t) =
  match user.inputs with
  | [ Ack ] ->
      let other_users =
        List.filter (Hashtbl.keys t.users) ~f:(fun username ->
            not (Username.equal user.username username))
      in

      [
        Page.Element.Text "Choose a player to rob";
        Choose_user { choose_this_many = 1; users = other_users };
      ]
  | [ Choose_user [ _ ]; Ack ] ->
      [ Text "Your new card"; Cards [ user.current_role ]; Ack_button ]
  | [ Ack; Choose_user _; Ack ] -> [ Text "Waiting" ]
  | _ -> [ Text "An error has occurred. Please start a new game" ]

let get_page_for_werewolf t (user : User.t) =
  match user.inputs with
  | [ Ack ] -> (
      let other_werewolves =
        Hashtbl.filter t.users ~f:(fun other_user ->
            match other_user.original_role with
            | Role.Werewolf ->
                not (Username.equal other_user.username user.username)
            | _ -> false)
        |> Hashtbl.keys
      in
      match other_werewolves with
      | [] ->
          let card = t.random_cache.werewolf_sees_card in
          let center_cards =
            List.filteri t.center_cards ~f:(fun i _ -> i = card)
          in
          [
            Page.Element.Text
              "There are no other werewolves. Card from the center:";
            Cards center_cards;
            Ack_button;
          ]
      | _ :: _ ->
          [
            Text "These are the other werewolves";
            Text (String.concat ~sep:", " other_werewolves);
            Ack_button;
          ] )
  | [ Ack; Ack ] -> [ Text "Waiting" ]
  | _ -> [ Text "An error has occurred. Please start a new game." ]

let get_page_for_user t (user : User.t) =
  match t.phase with
  | View_roles -> (
      match List.hd user.inputs with
      | Some Ack -> [ Page.Element.Text "Waiting" ]
      | _ ->
          [
            Page.Element.Text "This is your card";
            Cards [ user.original_role ];
            Ack_button;
          ] )
  | Day -> [ Text "Discuss" ]
  | Vote | Results -> [ Text "Not yet supported" ]
  | Night night_phase -> (
      if not (is_users_turn night_phase user) then [ Text "Waiting" ]
      else
        match user.original_role with
        | Robber -> get_page_for_robber t user
        | Werewolf -> get_page_for_werewolf t user )

let validate_input t (user : User.t) (input : Input.t) =
  match t.phase with
  | Day | Vote | Results -> false
  | View_roles -> (
      match (List.hd user.inputs, input) with None, Ack -> true | _ -> false )
  | Night phase -> (
      if not (is_users_turn phase user) then false
      else
        match user.original_role with
        | Robber -> (
            match (user.inputs, input) with
            | [ Ack ], Choose_user [ username ] -> Hashtbl.mem t.users username
            | [ Choose_user _; Ack ], Ack -> true
            | _ -> false )
        | Werewolf -> (
            match (user.inputs, input) with
            | [], Ack -> true
            | [ Ack ], Ack -> true
            | _ -> false ) )

let rec maybe_change_phase t =
  match t.phase with
  | View_roles ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            not (List.length user.inputs = 1))
        |> Hashtbl.is_empty
      in
      if phase_over then (
        t.phase <- Night Viewer;
        maybe_change_phase t )
  | Day | Vote | Results -> ()
  | Night (Troublemaker | Insomniac) ->
      t.phase <- Day;
      maybe_change_phase t
  | Night Viewer ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            match user.original_role with
            | Werewolf -> not (List.length user.inputs = 2)
            | _ -> false)
        |> Hashtbl.is_empty
      in
      if phase_over then (
        t.phase <- Night Robber;
        maybe_change_phase t )
  | Night Robber ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            match user.original_role with
            | Robber -> not (List.length user.inputs = 3)
            | _ -> false)
        |> Hashtbl.is_empty
      in
      if phase_over then (
        t.phase <- Day;
        maybe_change_phase t )

let on_input t username input =
  match Hashtbl.find t.users username with
  | None -> Or_error.error_string "Unknown user"
  | Some user ->
      if validate_input t user input then (
        user.inputs <- input :: user.inputs;
        ( match user.original_role with
        | Robber -> (
            match input with
            | Ack -> ()
            | Choose_user [ username ] -> (
                match Hashtbl.find t.users username with
                | None -> assert false
                | Some other_user ->
                    let other_role = other_user.current_role in
                    other_user.current_role <- user.current_role;
                    user.current_role <- other_role )
            | _ -> assert false )
        | Werewolf -> () );
        maybe_change_phase t;
        Ok () )
      else Or_error.error_string "Invalid input"

let get_page t username =
  match Hashtbl.find t.users username with
  | None -> [ Page.Element.Text "Page not found" ]
  | Some user -> get_page_for_user t user
