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
    mutable actions : string list;
  }

  let create username role =
    {
      username;
      inputs = [];
      original_role = role;
      current_role = role;
      actions = [];
    }

  let record_action t action =
    ksprintf (fun action -> t.actions <- t.actions @ [ action ]) action
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
  | Phase.Night.Viewer, (Werewolf | Seer | Mason) -> true
  | Robber, Robber -> true
  | Troublemaker, Troublemaker -> true
  | Insomniac, Insomniac -> true
  | _, _ -> false

let get_page_for_insomniac _t (user : User.t) =
  match user.inputs with
  | [ Ack ] ->
      [
        Page.Element.Centered_text "Your card now";
        Cards [ user.current_role ];
        Ack_button;
      ]
  | [ Ack; Ack ] -> [ Text "Waiting" ]
  | _ -> [ Text "An error has occurred. Please start a new game" ]

let get_page_for_mason t (user : User.t) =
  match user.inputs with
  | [ Ack ] -> (
      let other_masons =
        Hashtbl.filter t.users ~f:(fun other_user ->
            match other_user.original_role with
            | Role.Mason ->
                not (Username.equal other_user.username user.username)
            | _ -> false)
        |> Hashtbl.keys
      in
      match other_masons with
      | [] -> [ Page.Element.Text "There are no other masons."; Ack_button ]
      | [ mason ] -> [ Text (sprintf "The other mason is %s." mason) ]
      | masons ->
          [
            Text "These are the other masons:";
            Text (String.concat ~sep:", " masons);
          ] )
  | [ Ack; Ack ] -> [ Text "Waiting" ]
  | _ -> [ Text "An error has occurred. Please start a new game" ]

let get_page_for_troublemaker t (user : User.t) =
  match user.inputs with
  | [ Ack ] ->
      let other_users =
        List.filter (Hashtbl.keys t.users) ~f:(fun username ->
            not (Username.equal user.username username))
      in
      [
        Page.Element.Text "Choose two players to swap";
        Choose_user
          { choose_this_many = 2; users = other_users; or_center = false };
      ]
  | [ Choose_user _; Ack ] -> [ Text "Waiting" ]
  | _ -> [ Text "An error has occurred. Please start a new game" ]

let get_page_for_seer t (user : User.t) =
  match user.inputs with
  | [ Ack ] ->
      let other_users =
        List.filter (Hashtbl.keys t.users) ~f:(fun username ->
            not (Username.equal user.username username))
      in

      [
        Page.Element.Text
          "View another player's card, or two of the center cards.";
        Choose_user
          { choose_this_many = 1; users = other_users; or_center = true };
      ]
  | [ Choose_user [ username ]; Ack ] -> (
      match Hashtbl.find t.users username with
      | None -> [ Text "Seer chose nonexistent user. Please start a new game." ]
      | Some other_user ->
          [
            Text (sprintf "%s's card:" other_user.username);
            Cards [ other_user.current_role ];
            Ack_button;
          ] )
  | [ View_center_cards; Ack ] ->
      let center_cards =
        List.filteri t.center_cards ~f:(fun idx _ ->
            t.random_cache.seer_doesnt_see_card <> idx)
      in
      [ Text "Two of the center cards:"; Cards center_cards; Ack_button ]
  | [ Ack; (View_center_cards | Choose_user _); Ack ] -> [ Text "Waiting" ]
  | _ -> [ Text "An error has occurred. Please start a new game" ]

let get_page_for_robber t (user : User.t) =
  match user.inputs with
  | [ Ack ] ->
      let other_users =
        List.filter (Hashtbl.keys t.users) ~f:(fun username ->
            not (Username.equal user.username username))
      in

      [
        Page.Element.Text "Choose a player to rob";
        Choose_user
          { choose_this_many = 1; users = other_users; or_center = false };
      ]
  | [ Choose_user [ _ ]; Ack ] ->
      [ Centered_text "Your new card"; Cards [ user.current_role ]; Ack_button ]
  | [ Ack; Choose_user _; Ack ] -> [ Text "Waiting" ]
  | _ -> [ Text "An error has occurred. Please start a new game" ]

let what_werewolf_sees t (user : User.t) =
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
      let center_cards = List.filteri t.center_cards ~f:(fun i _ -> i = card) in
      `Center_cards center_cards
  | werewolves -> `Other_werewolves werewolves

let get_page_for_werewolf t (user : User.t) =
  match user.inputs with
  | [ Ack ] -> (
      match what_werewolf_sees t user with
      | `Center_cards center_cards ->
          [
            Page.Element.Text
              "There are no other werewolves. Card from the center:";
            Cards center_cards;
            Ack_button;
          ]
      | `Other_werewolves other_werewolves ->
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
            Page.Element.Centered_text "Your card";
            Cards [ user.original_role ];
            Ack_button;
          ] )
  | Day -> (
      match user.inputs with
      | Vote :: _ -> [ Page.Element.Text "Waiting" ]
      | _ ->
          let night_actions =
            [
              Page.Element.Text "During the night you:";
              Text
                (Html.to_string
                   (Html.ul []
                      (List.map user.actions ~f:(fun str ->
                           Html.li [] [ Html.text str ]))));
            ]
          in
          night_actions
          @ [
              Text "<hr></hr>";
              Text
                "Discuss. When you are ready to vote, hit the \"Ready to \
                 Vote\" button.";
              Vote_button;
            ] )
  | Vote -> (
      match user.inputs with
      | Choose_user [ _ ] :: _ -> [ Text "Waiting" ]
      | _ ->
          [
            Page.Element.Text "Vote:";
            Choose_user
              {
                choose_this_many = 1;
                users = Hashtbl.keys t.users;
                or_center = false;
              };
          ] )
  | Results ->
      let votes =
        Hashtbl.data t.users
        |> List.filter_map ~f:(fun user ->
               match user.inputs with
               | Choose_user [ username ] :: _ -> Some username
               | _ -> None)
        |> List.sort ~compare:String.compare
        |> List.group ~break:(fun s1 s2 -> not (String.equal s1 s2))
        |> List.map ~f:(fun l -> (List.hd_exn l, List.length l))
        |> List.sort ~compare:(fun (_, n1) (_, n2) -> Int.compare n2 n1)
      in
      let loser, losing_votes = List.hd_exn votes in
      let loser = Hashtbl.find_exn t.users loser in
      let other_votes =
        match votes with
        | [] | [ _ ] -> []
        | _ :: votes ->
            let users =
              List.map votes ~f:(fun (u, n) -> (Hashtbl.find_exn t.users u, n))
            in
            [
              Page.Element.Text "<hr></hr>";
              Text "Other players that received votes:";
              Text
                (Html.to_string
                   (Html.ul []
                      (List.map users ~f:(fun (user, votes) ->
                           Html.li []
                             [
                               Html.text
                                 (sprintf !"%s (%{Role}): %n vote%s"
                                    user.username user.current_role votes
                                    (if votes = 1 then "" else "s"));
                             ]))));
            ]
      in
      [
        Page.Element.Centered_text
          (sprintf
             !"%s (the %{Role}) received the most votes (%n)."
             loser.username loser.current_role losing_votes);
      ]
      @ other_votes
      @ [
          Text "<hr></hr>";
          Text "In the end, your cards were:";
          Text
            (Html.to_string
               (Html.ul []
                  (List.map (Hashtbl.data t.users) ~f:(fun user ->
                       Html.li []
                         [
                           Html.text
                             (sprintf !"%s: %{Role}" user.username
                                user.current_role);
                         ]))));
          No_refresh;
        ]
  | Night night_phase -> (
      if not (is_users_turn night_phase user) then [ Text "Waiting" ]
      else
        match user.original_role with
        | Robber -> get_page_for_robber t user
        | Werewolf -> get_page_for_werewolf t user
        | Seer -> get_page_for_seer t user
        | Villager -> [ Text "Waiting" ]
        | Troublemaker -> get_page_for_troublemaker t user
        | Mason -> get_page_for_mason t user
        | Insomniac -> get_page_for_insomniac t user )

let validate_input t (user : User.t) (input : Input.t) =
  match t.phase with
  | Day -> ( match input with Vote -> true | _ -> false )
  | Vote -> (
      match input with
      | Choose_user [ user ] -> Hashtbl.mem t.users user
      | _ -> false )
  | Results -> false
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
            | _ -> false )
        | Seer -> (
            match (user.inputs, input) with
            | [], Ack -> true
            | [ Ack ], Choose_user [ username ] -> Hashtbl.mem t.users username
            | [ Ack ], View_center_cards -> true
            | [ Choose_user _; Ack ], Ack -> true
            | [ View_center_cards; Ack ], Ack -> true
            | _ -> false )
        | Troublemaker -> (
            match (user.inputs, input) with
            | [], Ack -> true
            | [ Ack ], Choose_user [ user1; user2 ] ->
                Hashtbl.mem t.users user1 && Hashtbl.mem t.users user2
            | _ -> false )
        | Mason -> (
            match (user.inputs, input) with
            | [], Ack -> true
            | [ Ack ], Ack -> true
            | _ -> false )
        | Insomniac -> (
            match (user.inputs, input) with
            | [], Ack -> true
            | [ Ack ], Ack -> true
            | _ -> false )
        | Villager -> (
            match (user.inputs, input) with [], Ack -> true | _ -> false ) )

let record_action t (user : User.t) (input : Input.t) =
  match t.phase with
  | Day | Vote | Results -> ()
  | View_roles ->
      User.record_action user
        !"Saw that your original card was %{Role}"
        user.original_role
  | Night phase -> (
      if not (is_users_turn phase user) then ()
      else
        match user.original_role with
        | Robber -> (
            match (user.inputs, input) with
            | [ Ack ], Choose_user [ username ] ->
                User.record_action user !"Robbed %s" username
            | [ Choose_user _; Ack ], Ack ->
                User.record_action user
                  !"Saw that your new card was %{Role}"
                  user.current_role
            | _ -> () )
        | Werewolf -> (
            match (user.inputs, input) with
            | [ Ack ], Ack -> (
                match what_werewolf_sees t user with
                | `Center_cards [ c1 ] ->
                    User.record_action user
                      "Saw that you were the lone werewolf.";
                    User.record_action user !"Saw the %{Role} in the center" c1
                | `Center_cards _ -> ()
                | `Other_werewolves werewolves -> (
                    match werewolves with
                    | [ u1 ] ->
                        User.record_action user
                          "Saw that %s was the other werewolf" u1
                    | u1 :: users ->
                        User.record_action user
                          "Saw that %s and %s were the other werewolves"
                          (String.concat ~sep:", " users)
                          u1
                    | _ -> () ) )
            | _ -> () )
        | Seer -> (
            match (user.inputs, input) with
            | [ Ack ], Choose_user [ username ] ->
                User.record_action user "Chose to see %s's card" username
            | [ Ack ], View_center_cards ->
                User.record_action user "Chose to view two center cards"
            | [ Choose_user [ username ]; Ack ], Ack ->
                User.record_action user
                  !"Saw that %s's card was %{Role}"
                  username (Hashtbl.find_exn t.users username).current_role
            | [ View_center_cards; Ack ], Ack -> (
                let center_cards =
                  List.filteri t.center_cards ~f:(fun idx _ ->
                      t.random_cache.seer_doesnt_see_card <> idx)
                in
                match center_cards with
                | [ c1; c2 ] ->
                    User.record_action user
                      !"Saw two of the center cards: %{Role} and %{Role}"
                      c1 c2
                | _ -> () )
            | _ -> () )
        | Troublemaker -> (
            match (user.inputs, input) with
            | [ Ack ], Choose_user [ user1; user2 ] ->
                User.record_action user !"Swapped %s and %s" user1 user2
            | _ -> () )
        | Mason -> (
            match (user.inputs, input) with
            | [ Ack ], Ack -> (
                let other_masons =
                  Hashtbl.filter t.users ~f:(fun other_user ->
                      match other_user.original_role with
                      | Role.Mason ->
                          not (Username.equal other_user.username user.username)
                      | _ -> false)
                  |> Hashtbl.keys
                in
                match other_masons with
                | [] ->
                    User.record_action user
                      "Saw that there were no other masons"
                | [ mason ] ->
                    User.record_action user "Saw that the other mason was %s"
                      mason
                | m1 :: ms ->
                    User.record_action user
                      "Saw that the other masons were %s and %s"
                      (String.concat ms ~sep:", ")
                      m1 )
            | _ -> () )
        | Insomniac -> (
            match (user.inputs, input) with
            | [ Ack ], Ack ->
                User.record_action user
                  !"Saw that your card was now %{Role}"
                  user.current_role
            | _ -> () )
        | Villager -> () )

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
  | Day ->
      let phase_over =
        Hashtbl.for_all t.users ~f:(fun user ->
            match user.inputs with Vote :: _ -> true | _ -> false)
      in

      if phase_over then (
        t.phase <- Vote;
        maybe_change_phase t )
  | Vote ->
      let phase_over =
        Hashtbl.for_all t.users ~f:(fun user ->
            match user.inputs with Choose_user [ _ ] :: _ -> true | _ -> false)
      in
      if phase_over then (
        t.phase <- Results;
        maybe_change_phase t )
  | Results -> ()
  | Night Viewer ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            match user.original_role with
            | Werewolf -> not (List.length user.inputs = 2)
            | Seer -> not (List.length user.inputs = 3)
            | Mason -> not (List.length user.inputs = 2)
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
        t.phase <- Night Troublemaker;
        maybe_change_phase t )
  | Night Troublemaker ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            match user.original_role with
            | Troublemaker -> not (List.length user.inputs = 2)
            | _ -> false)
        |> Hashtbl.is_empty
      in
      if phase_over then (
        t.phase <- Night Insomniac;
        maybe_change_phase t )
  | Night Insomniac ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            match user.original_role with
            | Insomniac -> not (List.length user.inputs = 2)
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
        record_action t user input;
        user.inputs <- input :: user.inputs;
        ( match t.phase with
        | Night _ -> (
            match user.original_role with
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
            | Troublemaker -> (
                match input with
                | Ack -> ()
                | Choose_user [ user1; user2 ] -> (
                    match
                      (Hashtbl.find t.users user1, Hashtbl.find t.users user2)
                    with
                    | None, _ | _, None -> assert false
                    | Some user1, Some user2 ->
                        let role1 = user1.current_role in
                        user1.current_role <- user2.current_role;
                        user2.current_role <- role1 )
                | _ -> assert false )
            | Werewolf -> ()
            | Mason -> ()
            | Seer -> ()
            | Insomniac -> ()
            | Villager -> () )
        | _ -> () );
        maybe_change_phase t;
        Ok () )
      else Or_error.error_string "Invalid input"

let get_page t username =
  match Hashtbl.find t.users username with
  | None -> [ Page.Element.Text "Page not found" ]
  | Some user -> get_page_for_user t user
