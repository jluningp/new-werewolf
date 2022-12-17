open Core
open Shared

module Random_cache : sig
  type t

  val create : lone_werewolf_sees_non_werewolf_center_card:bool -> t
  val drunk_card : t -> Username.t -> int
  val voodoo_lou_card : t -> Username.t -> int

  val werewolf_sees_card :
    t -> Username.t -> non_werewolf_indices:int list -> int

  val seer_doesnt_see_card : t -> Username.t -> int
end = struct
  type t =
    { lone_werewolf_sees_non_werewolf_center_card : bool
    ; seer_doesnt_see_card : int Username.Table.t
    ; seer_doesnt_see_card__if_werewolf_card : int Username.Table.t
    ; werewolf_sees_card : int Username.Table.t
    ; drunk_card : int Username.Table.t
    ; voodoo_lou_card : int Username.Table.t }

  let create ~lone_werewolf_sees_non_werewolf_center_card =
    { lone_werewolf_sees_non_werewolf_center_card
    ; seer_doesnt_see_card = Username.Table.create ()
    ; seer_doesnt_see_card__if_werewolf_card = Username.Table.create ()
    ; werewolf_sees_card = Username.Table.create ()
    ; drunk_card = Username.Table.create ()
    ; voodoo_lou_card = Username.Table.create () }

  let voodoo_lou_card t user =
    Hashtbl.find_or_add t.voodoo_lou_card user ~default:(fun () -> Random.int 3)

  let drunk_card t user =
    Hashtbl.find_or_add t.drunk_card user ~default:(fun () -> Random.int 3)

  let werewolf_sees_card t user ~non_werewolf_indices =
    Hashtbl.find_or_add t.werewolf_sees_card user ~default:(fun () ->
        if t.lone_werewolf_sees_non_werewolf_center_card then
          List.nth_exn non_werewolf_indices
            (Random.int (List.length non_werewolf_indices))
        else Random.int 3 )

  let seer_doesnt_see_card t user =
    Hashtbl.find_or_add t.seer_doesnt_see_card user ~default:(fun () ->
        Random.int 3 )

  let rec gen_second_seer_card card1 =
    let card2 = Random.int 3 in
    if card1 = card2 then gen_second_seer_card card1 else card2

  let _seer_doesnt_see_card__if_werewolf_card t user =
    Hashtbl.find_or_add t.seer_doesnt_see_card__if_werewolf_card user
      ~default:(fun () ->
        let card1 = seer_doesnt_see_card t user in
        gen_second_seer_card card1 )
end

module Phase = struct
  module Night = struct
    type t =
      | Doppelganger
      | Doppelganger_seer
      | Doppelganger_robber
      | Doppelganger_troublemaker
      | Doppelganger_drunk
      | Viewer
      | Alpha_wolf
      | Robber of int
      | Voodoo_lou of int
      | Troublemaker of int
      | Dawn
    [@@deriving sexp_of]
  end

  type t = View_roles | Day | Night of Night.t | Vote | Results
  [@@deriving sexp_of]
end

module User = struct
  type t =
    { username : Username.t
    ; mutable inputs : Input.t list
    ; mutable original_role : Role.t
    ; mutable current_role : Role.t
    ; mutable actions : string list }

  let create username role =
    { username
    ; inputs = []
    ; original_role = role
    ; current_role = role
    ; actions = [] }

  let record_action t action =
    ksprintf (fun action -> t.actions <- t.actions @ [action]) action
end

type t =
  { users : User.t Username.Table.t
  ; random_cache : Random_cache.t
  ; mutable center_cards : Role.t list
  ; mutable center_werewolf_card : Role.t
  ; mutable phase : Phase.t
  ; admin : Username.t
  ; all_roles : Role.t list
  ; settings : Settings.t }

let center_card_username = "CENTER-CARD-231804952714"

let center_card_users =
  [center_card_username; center_card_username; center_card_username]

let assign_roles roles users =
  let users = List.permute (users @ center_card_users) in
  match List.zip users roles with
  | None -> Or_error.error_string "Incorrect number of roles."
  | Some role_assignments ->
      let center_cards, user_roles =
        List.partition_map role_assignments ~f:(fun (user, role) ->
            if String.equal user center_card_username then `Fst role
            else `Snd (user, role) )
      in
      Ok (center_cards, user_roles)

let create (settings : Settings.t) roles usernames =
  match assign_roles roles usernames with
  | Error err -> Error err
  | Ok (center_cards, user_roles) -> (
    match Username.Table.of_alist user_roles with
    | `Duplicate_key _ -> Or_error.error_string "Duplicate username"
    | `Ok users ->
        Ok
          { users =
              Hashtbl.mapi users ~f:(fun ~key ~data -> User.create key data)
          ; random_cache =
              Random_cache.create
                ~lone_werewolf_sees_non_werewolf_center_card:
                  settings.lone_werewolf_sees_non_werewolf_center_card
          ; phase = View_roles
          ; center_cards
          ; center_werewolf_card = Werewolf
          ; admin = List.last_exn usernames
          ; all_roles = roles
          ; settings } )

let is_users_turn night_phase (user : User.t) =
  match (night_phase, user.original_role) with
  | ( Phase.Night.Viewer
    , ( Werewolf | Mystic_wolf | Seer | Mason | Minion
      | Doppelganger (Some Werewolf)
      | Doppelganger (Some Mystic_wolf)
      | Doppelganger (Some Mason)
      | Doppelganger (Some Minion) ) ) ->
      true
  | Alpha_wolf, Alpha_wolf -> true
  | Robber n1, Robber n2 when n1 = n2 -> true
  | Troublemaker n1, Troublemaker n2 when n1 = n2 -> true
  | Voodoo_lou n1, Voodoo_lou n2 when n1 = n2 -> true
  | Dawn, (Drunk _ | Insomniac | Doppelganger (Some Insomniac)) -> true
  | Doppelganger, Doppelganger _ -> true
  | Doppelganger_seer, Doppelganger (Some Seer) -> true
  | Doppelganger_robber, Doppelganger (Some (Robber _)) -> true
  | Doppelganger_troublemaker, Doppelganger (Some (Troublemaker _)) -> true
  | Doppelganger_drunk, Doppelganger (Some (Drunk _)) -> true
  | _, _ -> false

let role_to_string_unnumbered_if_unique ?(ignore_settings = false)
    ?(never_number = false) t role =
  if ((not t.settings.show_role_number) && not ignore_settings) || never_number
  then Role.to_string_unnumbered role
  else
    let role_count =
      List.count t.all_roles ~f:(fun other_role ->
          match (role, other_role) with
          | Role.Doppelganger _, Doppelganger _ -> true
          | Robber _, Robber _ -> true
          | Troublemaker _, Troublemaker _ -> true
          | Drunk _, Drunk _ -> true
          | Voodoo_lou _, Voodoo_lou _ -> true
          | _, _ -> Role.equal role other_role )
    in
    if role_count = 1 then Role.to_string_unnumbered role
    else Role.to_string role

let get_page_for_insomniac t (user : User.t) =
  match user.inputs with
  | [Ack] ->
      [ Page.Element.Centered_text "Your card now"
      ; Cards [user.current_role]
      ; Centered_text
          ("The " ^ role_to_string_unnumbered_if_unique t user.current_role)
      ; Ack_button ]
  | [Ack; Ack] -> [Text "Waiting"]
  | _ -> [Text "An error has occurred. Please start a new game"]

let get_page_for_mason t (user : User.t) =
  match user.inputs with
  | [Ack] -> (
      let other_masons =
        Hashtbl.filter t.users ~f:(fun other_user ->
            match other_user.original_role with
            | Role.Mason | Role.Doppelganger (Some Mason) ->
                not (Username.equal other_user.username user.username)
            | _ -> false )
        |> Hashtbl.keys
      in
      match other_masons with
      | [] -> [Page.Element.Text "There are no other masons."; Ack_button]
      | [mason] -> [Text (sprintf "The other mason is %s." mason); Ack_button]
      | m :: masons ->
          [ Text
              (sprintf "%s and %s are the other masons"
                 (String.concat ~sep:", " masons)
                 m)
          ; Ack_button ] )
  | [Ack; Ack] -> [Text "Waiting"]
  | _ -> [Text "An error has occurred. Please start a new game"]

let get_page_for_troublemaker t (user : User.t) =
  match user.inputs with
  | [Ack] ->
      let other_users =
        List.filter (Hashtbl.keys t.users) ~f:(fun username ->
            not (Username.equal user.username username) )
      in
      [ Page.Element.Text "Choose two players to swap"
      ; Choose_user
          { choose_this_many = 2
          ; users = other_users
          ; or_center = false
          ; or_no_werewolf = false } ]
  | [Choose_user _; Ack] -> [Text "Waiting"]
  | _ -> [Text "An error has occurred. Please start a new game"]

let get_page_for_drunk t (user : User.t) =
  match user.inputs with
  | [Ack] ->
      [ Page.Element.Text
          "Your card was swapped with a center card. This is your new card:"
      ; Cards [user.current_role]
      ; Centered_text
          ("The " ^ role_to_string_unnumbered_if_unique t user.current_role)
      ; Ack_button ]
  | [Ack; Ack] -> [Page.Element.Text "Waiting"]
  | _ -> [Text "An error has occurred. Please start a new game"]

let get_page_for_seer t (user : User.t) =
  match user.inputs with
  | [Ack] ->
      let other_users =
        List.filter (Hashtbl.keys t.users) ~f:(fun username ->
            not (Username.equal user.username username) )
      in
      [ Page.Element.Text
          "View another player's card, or two of the center cards."
      ; Choose_user
          { choose_this_many = 1
          ; users = other_users
          ; or_center = true
          ; or_no_werewolf = false } ]
  | [Choose_user [username]; Ack] -> (
    match Hashtbl.find t.users username with
    | None -> [Text "Seer chose nonexistent user. Please start a new game."]
    | Some other_user ->
        [ Text (sprintf "%s's card:" other_user.username)
        ; Cards [other_user.current_role]
        ; Centered_text
            ( "The "
            ^ role_to_string_unnumbered_if_unique t other_user.current_role )
        ; Ack_button ] )
  | [View_center_cards; Ack] ->
      let center_cards =
        List.filteri t.center_cards ~f:(fun idx _ ->
            Random_cache.seer_doesnt_see_card t.random_cache user.username
            <> idx )
      in
      [Page.Element.Text "Two of the center cards:"; Cards center_cards]
      @ List.map center_cards ~f:(fun card ->
            Page.Element.Centered_text
              ("The " ^ role_to_string_unnumbered_if_unique t card) )
      @ [Page.Element.Ack_button]
  | [Ack; (View_center_cards | Choose_user _); Ack] -> [Text "Waiting"]
  | _ -> [Text "An error has occurred. Please start a new game"]

let get_page_for_robber t (user : User.t) =
  match user.inputs with
  | [Ack] ->
      let other_users =
        List.filter (Hashtbl.keys t.users) ~f:(fun username ->
            not (Username.equal user.username username) )
      in
      [ Page.Element.Text "Choose a player to rob"
      ; Choose_user
          { choose_this_many = 1
          ; users = other_users
          ; or_center = false
          ; or_no_werewolf = false } ]
  | [Choose_user [_]; Ack] ->
      [ Centered_text "Your new card"
      ; Cards [user.current_role]
      ; Centered_text
          ("The " ^ role_to_string_unnumbered_if_unique t user.current_role)
      ; Ack_button ]
  | [Ack; Choose_user _; Ack] -> [Text "Waiting"]
  | _ -> [Text "An error has occurred. Please start a new game"]

let what_werewolf_sees t (user : User.t) =
  let other_werewolves =
    Hashtbl.filter t.users ~f:(fun other_user ->
        match other_user.original_role with
        | Role.Werewolf | Mystic_wolf | Dream_wolf
         |Doppelganger (Some Werewolf | Some Mystic_wolf | Some Dream_wolf) ->
            not (Username.equal other_user.username user.username)
        | _ -> false )
    |> Hashtbl.keys
  in
  match other_werewolves with
  | [] ->
      let non_werewolf_indices =
        let indices =
          List.filter_mapi t.center_cards ~f:(fun i card ->
              if Role.equal Werewolf card then None else Some i )
        in
        (* If all center cards are werewolves, give up and allow
           the werewolf to see a werewolf. *)
        match indices with
        | [] -> List.mapi t.center_cards ~f:(fun i _ -> i)
        | _ -> indices
      in
      let card =
        Random_cache.werewolf_sees_card t.random_cache user.username
          ~non_werewolf_indices
      in
      let center_cards =
        List.filteri t.center_cards ~f:(fun i _ -> i = card)
      in
      `Center_cards center_cards
  | werewolves -> `Other_werewolves werewolves

let get_page_for_minion t (user : User.t) =
  match user.inputs with
  | [Ack] -> (
    match what_werewolf_sees t user with
    | `Other_werewolves werewolves -> (
      match werewolves with
      | [] ->
          [Page.Element.Text "An error has occurred. Please start a new game."]
      | [u1] -> [Text (sprintf "%s is the werewolf" u1); Ack_button]
      | u1 :: users ->
          [ Text
              (sprintf "%s and %s are the werewolves"
                 (String.concat ~sep:", " users)
                 u1)
          ; Ack_button ] )
    | `Center_cards _ -> [Text "There are no werewolves."; Ack_button] )
  | [Ack; Ack] -> [Text "Waiting"]
  | _ -> [Text "An error has occurred. Please start a new game."]

let get_page_for_voodoo_lou t (user : User.t) =
  match user.inputs with
  | [Ack] ->
      let center_card =
        Random_cache.voodoo_lou_card t.random_cache user.username
        |> List.nth_exn t.center_cards
      in
      [ Page.Element.Text
          "Swap this card from the center with your own card or another \
           player's card:"
      ; Cards [center_card]
      ; Centered_text
          ("The " ^ role_to_string_unnumbered_if_unique t center_card)
      ; Choose_user
          { choose_this_many = 1
          ; users = Hashtbl.keys t.users
          ; or_center = false
          ; or_no_werewolf = false } ]
  | [Choose_user _; Ack] -> [Text "Waiting"]
  | _ -> [Text "An error has occurred. Please start a new game."]

let get_page_for_werewolf t (user : User.t) =
  match user.inputs with
  | [Ack] -> (
    match what_werewolf_sees t user with
    | `Center_cards center_cards ->
        [ Page.Element.Text
            "There are no other werewolves. Card from the center:"
        ; Cards center_cards ]
        @ List.map center_cards ~f:(fun card ->
              Page.Element.Centered_text
                ("The " ^ role_to_string_unnumbered_if_unique t card) )
        @ [Page.Element.Ack_button]
    | `Other_werewolves other_werewolves -> (
      match other_werewolves with
      | [] ->
          [Page.Element.Text "An error has occurred. Please start a new game."]
      | [u1] -> [Text (sprintf "%s is the other werewolf" u1); Ack_button]
      | u1 :: users ->
          [ Text
              (sprintf "%s and %s are the other werewolves"
                 (String.concat ~sep:", " users)
                 u1)
          ; Ack_button ] ) )
  | [Ack; Ack] -> [Text "Waiting"]
  | _ -> [Text "An error has occurred. Please start a new game."]

let get_page_for_mystic_wolf t (user : User.t) =
  match user.inputs with
  | [Ack] -> get_page_for_werewolf t user
  | [Ack; Ack] ->
      [ Page.Element.Text "Look at another player's card."
      ; Choose_user
          { choose_this_many = 1
          ; or_center = false
          ; or_no_werewolf = false
          ; users =
              Hashtbl.keys t.users
              |> List.filter ~f:(fun username ->
                     not (String.equal user.username username) ) } ]
  | [Choose_user [username]; Ack; Ack] ->
      let other_player = Hashtbl.find_exn t.users username in
      [ Text (sprintf "%s's role is:" username)
      ; Cards [other_player.current_role]
      ; Centered_text
          ( "The "
          ^ role_to_string_unnumbered_if_unique t other_player.current_role )
      ; Ack_button ]
  | [Ack; Choose_user _; Ack; Ack] -> [Text "Waiting"]
  | _ -> [Text "An error has occurred. Please start a new game."]

let get_page_for_alpha_wolf t (user : User.t) =
  match user.inputs with
  | [Ack] -> get_page_for_werewolf t user
  | [Ack; Ack] ->
      let other_users =
        List.filter (Hashtbl.keys t.users) ~f:(fun username ->
            not (Username.equal user.username username) )
      in
      [ Page.Element.Text
          "Choose another player. Their card will be swapped with the center \
           werewolf card."
      ; Choose_user
          { choose_this_many = 1
          ; users = other_users
          ; or_center = false
          ; or_no_werewolf = false } ]
  | [Choose_user _; Ack; Ack] -> [Text "Waiting"]
  | _ -> [Text "An error has occurred. Please start a new game"]

let get_page_for_doppleganger t (user : User.t) =
  match user.inputs with
  | [Ack] ->
      [ Page.Element.Text
          "Look at another player's card. You will become that role."
      ; Choose_user
          { choose_this_many = 1
          ; or_center = false
          ; or_no_werewolf = false
          ; users =
              Hashtbl.keys t.users
              |> List.filter ~f:(fun username ->
                     not (String.equal user.username username) ) } ]
  | [Choose_user _; Ack] ->
      let role =
        match user.current_role with
        | Doppelganger (Some role) -> role
        | _ -> assert false
      in
      [ Text "You are now this role:"
      ; Cards [role]
      ; Centered_text ("The " ^ role_to_string_unnumbered_if_unique t role)
      ; Ack_button ]
  | inputs -> (
    match List.rev inputs with
    | Ack :: Choose_user _ :: Ack :: rest -> (
        let inputs = List.rev (Input.Ack :: rest) in
        match user.original_role with
        | Doppelganger (Some role) -> (
            let user = {user with original_role = role; inputs} in
            match role with
            | Robber _ -> get_page_for_robber t user
            | Werewolf -> get_page_for_werewolf t user
            | Mystic_wolf -> get_page_for_mystic_wolf t user
            | Alpha_wolf -> get_page_for_alpha_wolf t user
            | Minion -> get_page_for_minion t user
            | Seer -> get_page_for_seer t user
            | Villager | Tanner | Hunter | Dream_wolf -> [Text "Waiting"]
            | Troublemaker _ -> get_page_for_troublemaker t user
            | Mason -> get_page_for_mason t user
            | Insomniac -> get_page_for_insomniac t user
            | Drunk _ -> get_page_for_drunk t user
            | Voodoo_lou _ -> get_page_for_voodoo_lou t user
            | Doppelganger _ ->
                [Text "An error has occurred. Please start a new game."] )
        | _ -> [Text "An error has occurred. Please start a new game."] )
    | _ -> [Text "An error has occurred. Please start a new game."] )

let get_results_page t (user : User.t) =
  let votes =
    Hashtbl.data t.users
    |> List.filter_map ~f:(fun user ->
           match user.inputs with
           | Choose_user [username] :: _ -> Some username
           | Choose_no_werewolf :: _ -> Some "no__werewolf__"
           | _ -> None )
    |> List.sort ~compare:String.compare
    |> List.group ~break:(fun s1 s2 -> not (String.equal s1 s2))
    |> List.map ~f:(fun l -> (List.hd_exn l, List.length l))
    |> List.sort ~compare:(fun (_, n1) (_, n2) -> Int.compare n2 n1)
    |> List.map ~f:(fun (username, count) ->
           match username with
           | "no__werewolf__" -> ("\"No werewolf\"", None, count)
           | _ ->
               let user = Hashtbl.find_exn t.users username in
               (username, Some user.current_role, count) )
  in
  let loser, loser_role, losing_votes = List.hd_exn votes in
  let role_to_string ?ignore_settings role =
    match role with
    | None -> ""
    | Some (Role.Doppelganger (Some role)) ->
        sprintf !"(the Doppelganger-%s)"
          (role_to_string_unnumbered_if_unique ?ignore_settings t role)
    | Some role ->
        sprintf "(the %s)"
          (role_to_string_unnumbered_if_unique ?ignore_settings t role)
  in
  let hunter_vote =
    let votes =
      Hashtbl.data t.users
      |> List.filter_map ~f:(fun user ->
             match user.current_role with
             | Hunter -> (
               match user.inputs with
               | Choose_user [username] :: _ -> Some (user.username, username)
               | Choose_no_werewolf :: _ ->
                   Some (user.username, "\"No Werewolf\"")
               | _ -> None )
             | _ -> None )
    in
    match votes with
    | [] -> []
    | [(username, vote)] ->
        [ Page.Element.Text
            (sprintf "The hunter (%s) voted for: %s" username vote) ]
    | votes ->
        [ Page.Element.Text "The hunters voted for:"
        ; Text
            (Html.to_string
               (Html.ul []
                  (List.map votes ~f:(fun (user, vote) ->
                       Html.li []
                         [Html.text (sprintf "Hunter %s: %s" user vote)] ))))
        ]
  in
  let other_votes =
    match votes with
    | [] | [_] -> []
    | _ :: votes ->
        [ Page.Element.Text "<hr></hr>"
        ; Text "Other players that received votes:"
        ; Text
            (Html.to_string
               (Html.ul []
                  (List.map votes ~f:(fun (user, role, votes) ->
                       Html.li []
                         [ Html.text
                             (sprintf !"%s %s: %n vote%s" user
                                (role_to_string role) votes
                                (if votes = 1 then "" else "s")) ] )))) ]
  in
  let is_alpha_wolf =
    List.exists (Hashtbl.data t.users) ~f:(fun user ->
        Role.equal user.current_role Alpha_wolf )
    || Role.equal t.center_werewolf_card Alpha_wolf
    || List.exists t.center_cards ~f:(Role.equal Alpha_wolf)
  in
  [ Page.Element.Centered_text
      (sprintf
         !"%s %s received the most votes (%n)."
         loser
         (role_to_string loser_role)
         losing_votes) ]
  @ hunter_vote @ other_votes
  @ [ Page.Element.Text "<hr></hr>"
    ; Text "In the end, your cards were:"
    ; Text
        (Html.to_string
           (Html.ul []
              (List.map (Hashtbl.data t.users) ~f:(fun user ->
                   Html.li []
                     [ Html.text
                         (sprintf !"%s: %s" user.username
                            (role_to_string (Some user.current_role)
                               ~ignore_settings:true)) ] ))))
    ; Text
        (sprintf
           !"Center cards: %s, and %s"
           (String.concat ~sep:", "
              (List.map
                 ~f:
                   (role_to_string_unnumbered_if_unique t ~ignore_settings:true)
                 (List.tl_exn t.center_cards)))
           (role_to_string_unnumbered_if_unique t
              (List.hd_exn t.center_cards)
              ~ignore_settings:true)) ]
  @ ( if is_alpha_wolf then
      [ Page.Element.Text
          (sprintf
             !"Center werewolf card: %s"
             (role_to_string_unnumbered_if_unique t t.center_werewolf_card
                ~ignore_settings:true)) ]
    else [] )
  @
  if String.equal user.username t.admin then
    [ Page.Element.No_refresh
    ; Html
        "<div style=\"text-align:center;\"><div id=button \
         onclick=\"newGame()\">New Game</div></div>" ]
  else []

let get_page_for_user t (user : User.t) =
  match t.phase with
  | View_roles -> (
    match List.hd user.inputs with
    | Some Ack -> [Page.Element.Text "Waiting"]
    | _ ->
        [ Page.Element.Centered_text "Your card"
        ; Cards [user.original_role]
        ; Centered_text
            ("The " ^ role_to_string_unnumbered_if_unique t user.original_role)
        ; Ack_button ] )
  | Day -> (
    match user.inputs with
    | Vote :: _ -> [Page.Element.Text "Waiting"]
    | _ ->
        let night_actions =
          [ Page.Element.Text "During the night you:"
          ; Text
              (Html.to_string
                 (Html.ul []
                    (List.map user.actions ~f:(fun str ->
                         Html.li [] [Html.text str] )))) ]
        in
        let roles =
          let user_roles =
            Hashtbl.data t.users |> List.map ~f:(fun user -> user.current_role)
          in
          t.center_cards @ user_roles
          |> List.map
               ~f:(role_to_string_unnumbered_if_unique t ~never_number:true)
          |> List.map ~f:(fun r -> (r, ()))
          |> String.Map.of_alist_multi |> Map.map ~f:List.length
          |> Map.to_alist
        in
        night_actions
        @ [ Text "<hr></hr>"
          ; Text "These are the roles currently in play: "
          ; Text
              (Html.to_string
                 (Html.ul []
                    (List.map roles ~f:(fun (role, count) ->
                         Html.li [] [Html.text (sprintf "%s: %n" role count)]
                     ))))
          ; Text "<hr></hr>"
          ; Text
              "Discuss. When you are ready to vote, hit the \"Ready to Vote\" \
               button."
          ; Vote_button ] )
  | Vote -> (
    match user.inputs with
    | Choose_user [_] :: _ -> [Text "Waiting"]
    | Choose_no_werewolf :: _ -> [Text "Waiting"]
    | _ ->
        [ Page.Element.Text "Vote:"
        ; Choose_user
            { choose_this_many = 1
            ; users = Hashtbl.keys t.users
            ; or_center = false
            ; or_no_werewolf = true } ] )
  | Results -> get_results_page t user
  | Night night_phase -> (
      if not (is_users_turn night_phase user) then [Text "Waiting"]
      else
        match user.original_role with
        | Robber _ -> get_page_for_robber t user
        | Werewolf -> get_page_for_werewolf t user
        | Minion -> get_page_for_minion t user
        | Seer -> get_page_for_seer t user
        | Villager | Tanner | Hunter | Dream_wolf -> [Text "Waiting"]
        | Troublemaker _ -> get_page_for_troublemaker t user
        | Mason -> get_page_for_mason t user
        | Insomniac -> get_page_for_insomniac t user
        | Drunk _ -> get_page_for_drunk t user
        | Mystic_wolf -> get_page_for_mystic_wolf t user
        | Alpha_wolf -> get_page_for_alpha_wolf t user
        | Voodoo_lou _ -> get_page_for_voodoo_lou t user
        | Doppelganger _ -> get_page_for_doppleganger t user )

let validate_input t (user : User.t) (input : Input.t) =
  match t.phase with
  | Day -> ( match input with Vote -> true | _ -> false )
  | Vote -> (
    match input with
    | Choose_user [user] -> Hashtbl.mem t.users user
    | Choose_no_werewolf -> true
    | _ -> false )
  | Results -> false
  | View_roles -> (
    match (List.hd user.inputs, input) with None, Ack -> true | _ -> false )
  | Night phase -> (
      if not (is_users_turn phase user) then false
      else
        let user_inputs =
          match user.original_role with
          | Doppelganger (Some _) -> (
            match List.rev user.inputs with
            | Ack :: Choose_user [_] :: inputs -> List.rev inputs
            | _ -> assert false )
          | _ -> user.inputs
        in
        let original_role =
          match user.original_role with
          | Doppelganger (Some role) -> role
          | role -> role
        in
        match original_role with
        | Robber _ -> (
          match (user_inputs, input) with
          | [], Ack -> true
          | [Ack], Choose_user [username] -> Hashtbl.mem t.users username
          | [Choose_user _; Ack], Ack -> true
          | _ -> false )
        | Seer -> (
          match (user_inputs, input) with
          | [], Ack -> true
          | [Ack], Choose_user [username] -> Hashtbl.mem t.users username
          | [Ack], View_center_cards -> true
          | [Choose_user _; Ack], Ack -> true
          | [View_center_cards; Ack], Ack -> true
          | _ -> false )
        | Voodoo_lou _ -> (
          match (user_inputs, input) with
          | [], Ack -> true
          | [Ack], Choose_user [username] -> Hashtbl.mem t.users username
          | _ -> false )
        | Troublemaker _ -> (
          match (user_inputs, input) with
          | [], Ack -> true
          | [Ack], Choose_user [user1; user2] ->
              Hashtbl.mem t.users user1 && Hashtbl.mem t.users user2
          | _ -> false )
        | Werewolf | Mason | Insomniac | Drunk _ | Minion -> (
          match (user_inputs, input) with
          | [], Ack -> true
          | [Ack], Ack -> true
          | _ -> false )
        | Mystic_wolf -> (
          match (user_inputs, input) with
          | [], Ack -> true
          | [Ack], Ack -> true
          | [Ack; Ack], Choose_user [username] -> Hashtbl.mem t.users username
          | [Choose_user _; Ack; Ack], Ack -> true
          | _ -> false )
        | Alpha_wolf -> (
          match (user_inputs, input) with
          | [], Ack -> true
          | [Ack], Ack -> true
          | [Ack; Ack], Choose_user [username] -> Hashtbl.mem t.users username
          | _ -> false )
        | Villager | Tanner | Hunter | Dream_wolf -> (
          match (user_inputs, input) with [], Ack -> true | _ -> false )
        | Doppelganger None -> (
          match (user_inputs, input) with
          | [], Ack -> true
          | [Ack], Choose_user [user] -> Hashtbl.mem t.users user
          | _ -> false )
        | Doppelganger (Some _) -> false )

let record_werewolf_action t user user_inputs input =
  match (user_inputs, input) with
  | [Input.Ack], Input.Ack -> (
    match what_werewolf_sees t user with
    | `Center_cards [c1] ->
        User.record_action user "Saw that you were the lone werewolf." ;
        User.record_action user
          !"Saw the %s in the center"
          (role_to_string_unnumbered_if_unique t c1)
    | `Center_cards _ -> ()
    | `Other_werewolves werewolves -> (
      match werewolves with
      | [u1] -> User.record_action user "Saw that %s was the other werewolf" u1
      | u1 :: users ->
          User.record_action user
            "Saw that %s and %s were the other werewolves"
            (String.concat ~sep:", " users)
            u1
      | _ -> () ) )
  | _ -> ()

let record_action t (user : User.t) (input : Input.t) =
  match t.phase with
  | Day | Vote | Results -> ()
  | View_roles ->
      User.record_action user
        !"Saw that your original card was %s"
        (role_to_string_unnumbered_if_unique t user.original_role)
  | Night phase -> (
      if not (is_users_turn phase user) then ()
      else
        let user_inputs =
          match user.original_role with
          | Doppelganger (Some _) -> (
            match List.rev user.inputs with
            | Ack :: Choose_user [_] :: inputs -> List.rev inputs
            | _ -> assert false )
          | _ -> user.inputs
        in
        let original_role =
          match user.original_role with
          | Doppelganger (Some role) -> role
          | role -> role
        in
        match original_role with
        | Robber _ -> (
          match (user_inputs, input) with
          | [Ack], Choose_user [username] ->
              User.record_action user !"Robbed %s" username
          | [Choose_user _; Ack], Ack ->
              User.record_action user
                !"Saw that your new card was %s"
                (role_to_string_unnumbered_if_unique t user.current_role)
          | _ -> () )
        | Werewolf -> record_werewolf_action t user user_inputs input
        | Mystic_wolf -> (
            record_werewolf_action t user user_inputs input ;
            match (user_inputs, input) with
            | [Ack; Ack], Choose_user [username] ->
                let other_user = Hashtbl.find_exn t.users username in
                User.record_action user !"Saw that %s was the %s" username
                  (role_to_string_unnumbered_if_unique t
                     other_user.current_role)
            | _ -> () )
        | Alpha_wolf -> (
            record_werewolf_action t user user_inputs input ;
            match (user_inputs, input) with
            | [Ack; Ack], Choose_user [username] ->
                User.record_action user
                  !"Swapped %s's card with the center werewolf card"
                  username
            | _ -> () )
        | Minion -> (
          match (user_inputs, input) with
          | [Ack], Ack -> (
            match what_werewolf_sees t user with
            | `Center_cards _ ->
                User.record_action user "Saw that there were no werewolves"
            | `Other_werewolves werewolves -> (
              match werewolves with
              | [u1] ->
                  User.record_action user "Saw that %s was the werewolf" u1
              | u1 :: users ->
                  User.record_action user
                    "Saw that %s and %s were the werewolves"
                    (String.concat ~sep:", " users)
                    u1
              | _ -> () ) )
          | _ -> () )
        | Seer -> (
          match (user_inputs, input) with
          | [Ack], Choose_user [username] ->
              User.record_action user "Chose to see %s's card" username
          | [Ack], View_center_cards ->
              User.record_action user "Chose to view two center cards"
          | [Choose_user [username]; Ack], Ack ->
              User.record_action user
                !"Saw that %s's card was %s"
                username
                (role_to_string_unnumbered_if_unique t
                   (Hashtbl.find_exn t.users username).current_role)
          | [View_center_cards; Ack], Ack -> (
              let center_cards =
                List.filteri t.center_cards ~f:(fun idx _ ->
                    Random_cache.seer_doesnt_see_card t.random_cache
                      user.username
                    <> idx )
              in
              match center_cards with
              | [c1; c2] ->
                  User.record_action user
                    !"Saw two of the center cards: %s and %s"
                    (role_to_string_unnumbered_if_unique t c1)
                    (role_to_string_unnumbered_if_unique t c2)
              | _ -> () )
          | _ -> () )
        | Troublemaker _ -> (
          match (user_inputs, input) with
          | [Ack], Choose_user [user1; user2] ->
              User.record_action user !"Swapped %s and %s" user1 user2
          | _ -> () )
        | Voodoo_lou _ -> (
          match (user_inputs, input) with
          | [Ack], Choose_user [swapped_with] ->
              let center_card =
                Random_cache.voodoo_lou_card t.random_cache user.username
                |> List.nth_exn t.center_cards
              in
              let who =
                if Username.equal swapped_with user.username then "your own"
                else sprintf "%s's" swapped_with
              in
              User.record_action user
                !"Swapped %s card with a center card (the %s)."
                who
                (role_to_string_unnumbered_if_unique t center_card)
          | _ -> () )
        | Mason -> (
          match (user_inputs, input) with
          | [Ack], Ack -> (
              let other_masons =
                Hashtbl.filter t.users ~f:(fun other_user ->
                    match other_user.original_role with
                    | Role.Mason | Role.Doppelganger (Some Mason) ->
                        not (Username.equal other_user.username user.username)
                    | _ -> false )
                |> Hashtbl.keys
              in
              match other_masons with
              | [] ->
                  User.record_action user "Saw that there were no other masons"
              | [mason] ->
                  User.record_action user "Saw that the other mason was %s"
                    mason
              | m1 :: ms ->
                  User.record_action user
                    "Saw that the other masons were %s and %s"
                    (String.concat ms ~sep:", ")
                    m1 )
          | _ -> () )
        | Insomniac -> (
          match (user_inputs, input) with
          | [Ack], Ack ->
              User.record_action user
                !"Saw that your card was now %s"
                (role_to_string_unnumbered_if_unique t user.current_role)
          | _ -> () )
        | Drunk _ -> (
          match (user_inputs, input) with
          | [Ack], Ack ->
              User.record_action user "Swapped your card with a center card" ;
              User.record_action user
                !"Saw that your card was now %s"
                (role_to_string_unnumbered_if_unique t user.current_role)
          | _ -> () )
        | Doppelganger None -> (
          match (user_inputs, input) with
          | [Ack], Choose_user [username] ->
              User.record_action user
                !"Assumed %s's role: the %s"
                username
                (role_to_string_unnumbered_if_unique t
                   (Hashtbl.find_exn t.users username).current_role)
          | _ -> () )
        | Doppelganger (Some _) | Villager | Tanner | Hunter | Dream_wolf -> ()
      )

let swap_drunks ?(doppleganger = false) t =
  let users = Hashtbl.data t.users in
  let drunks =
    List.filter users ~f:(fun user ->
        if doppleganger then
          match user.original_role with
          | Role.Doppelganger (Some (Drunk _)) -> true
          | _ -> false
        else match user.original_role with Role.Drunk _ -> true | _ -> false )
    |> List.sort ~compare:(fun u1 u2 ->
           match (u1.User.original_role, u2.User.original_role) with
           | Role.Drunk n1, Role.Drunk n2 -> Int.compare n1 n2
           | _ -> 0 )
  in
  List.iter drunks ~f:(fun drunk ->
      let drunk_card = Random_cache.drunk_card t.random_cache drunk.username in
      let card = List.nth_exn t.center_cards drunk_card in
      t.center_cards
      <- List.mapi t.center_cards ~f:(fun i card ->
             if i = drunk_card then drunk.current_role else card ) ;
      drunk.current_role <- card )

let rec maybe_change_phase t =
  match t.phase with
  | View_roles ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user -> not (List.length user.inputs = 1)
        )
        |> Hashtbl.is_empty
      in
      if phase_over then (
        t.phase <- Night Doppelganger ;
        maybe_change_phase t )
  | Day ->
      let phase_over =
        Hashtbl.for_all t.users ~f:(fun user ->
            match user.inputs with Vote :: _ -> true | _ -> false )
      in
      if phase_over then (
        t.phase <- Vote ;
        maybe_change_phase t )
  | Vote ->
      let phase_over =
        Hashtbl.for_all t.users ~f:(fun user ->
            match user.inputs with
            | (Choose_user [_] | Choose_no_werewolf) :: _ -> true
            | _ -> false )
      in
      if phase_over then (
        t.phase <- Results ;
        maybe_change_phase t )
  | Results -> ()
  | Night Doppelganger ->
      let phase_over =
        Hashtbl.for_all t.users ~f:(fun user ->
            match user.original_role with
            | Doppelganger _ -> List.length user.inputs = 3
            | _ -> true )
      in
      if phase_over then (
        t.phase <- Night Doppelganger_seer ;
        maybe_change_phase t )
  | Night Doppelganger_seer ->
      let phase_over =
        Hashtbl.for_all t.users ~f:(fun user ->
            match user.original_role with
            | Doppelganger (Some Seer) -> List.length user.inputs = 5
            | _ -> true )
      in
      if phase_over then (
        t.phase <- Night Doppelganger_robber ;
        maybe_change_phase t )
  | Night Doppelganger_robber ->
      let phase_over =
        Hashtbl.for_all t.users ~f:(fun user ->
            match user.original_role with
            | Doppelganger (Some (Robber _)) -> List.length user.inputs = 5
            | _ -> true )
      in
      if phase_over then (
        t.phase <- Night Doppelganger_troublemaker ;
        maybe_change_phase t )
  | Night Doppelganger_troublemaker ->
      let phase_over =
        Hashtbl.for_all t.users ~f:(fun user ->
            match user.original_role with
            | Doppelganger (Some (Troublemaker _)) ->
                List.length user.inputs = 4
            | _ -> true )
      in
      if phase_over then (
        swap_drunks t ~doppleganger:true ;
        t.phase <- Night Doppelganger_drunk ;
        maybe_change_phase t )
  | Night Doppelganger_drunk ->
      let phase_over =
        Hashtbl.for_all t.users ~f:(fun user ->
            match user.original_role with
            | Doppelganger (Some (Drunk _)) -> List.length user.inputs = 4
            | _ -> true )
      in
      if phase_over then (
        t.phase <- Night Alpha_wolf ;
        maybe_change_phase t )
  | Night Viewer ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            match user.original_role with
            | Werewolf -> not (List.length user.inputs = 2)
            | Mystic_wolf -> not (List.length user.inputs = 4)
            | Seer -> not (List.length user.inputs = 3)
            | Mason -> not (List.length user.inputs = 2)
            | Minion -> not (List.length user.inputs = 2)
            | Doppelganger (Some (Werewolf | Mason | Minion)) ->
                not (List.length user.inputs = 4)
            | Doppelganger _ | Robber _ | Troublemaker _ | Villager
             |Insomniac | Tanner | Drunk _ | Hunter | Dream_wolf | Alpha_wolf
             |Voodoo_lou _ ->
                false )
        |> Hashtbl.is_empty
      in
      if phase_over then (
        t.phase <- Night (Robber 0) ;
        maybe_change_phase t )
  | Night (Robber n) ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            match user.original_role with
            | Robber n' when n' = n -> not (List.length user.inputs = 3)
            | _ -> false )
        |> Hashtbl.is_empty
      in
      let robber_count =
        List.count t.all_roles ~f:(fun role ->
            match role with Robber _ -> true | _ -> false )
      in
      let phase =
        if n >= robber_count - 1 then Phase.Night (Voodoo_lou 0)
        else Night (Robber (n + 1))
      in
      if phase_over then (
        t.phase <- phase ;
        maybe_change_phase t )
  | Night (Voodoo_lou n) ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            match user.original_role with
            | Voodoo_lou n' when n' = n -> not (List.length user.inputs = 2)
            | _ -> false )
        |> Hashtbl.is_empty
      in
      let vl_count =
        List.count t.all_roles ~f:(fun role ->
            match role with Voodoo_lou _ -> true | _ -> false )
      in
      let phase =
        if n >= vl_count - 1 then Phase.Night (Troublemaker 0)
        else Night (Voodoo_lou (n + 1))
      in
      if phase_over then (
        t.phase <- phase ;
        maybe_change_phase t )
  | Night Alpha_wolf ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            match user.original_role with
            | Alpha_wolf -> not (List.length user.inputs = 3)
            | _ -> false )
        |> Hashtbl.is_empty
      in
      if phase_over then (
        t.phase <- Night Viewer ;
        maybe_change_phase t )
  | Night (Troublemaker n) ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            match user.original_role with
            | Troublemaker n' when n' = n -> not (List.length user.inputs = 2)
            | _ -> false )
        |> Hashtbl.is_empty
      in
      let troublemaker_count =
        List.count t.all_roles ~f:(fun role ->
            match role with Troublemaker _ -> true | _ -> false )
      in
      let phase =
        if n >= troublemaker_count - 1 then Phase.Night Dawn
        else Night (Troublemaker (n + 1))
      in
      if phase_over then (
        (match phase with Night Dawn -> swap_drunks t | _ -> ()) ;
        t.phase <- phase ;
        maybe_change_phase t )
  | Night Dawn ->
      let phase_over =
        Hashtbl.filter t.users ~f:(fun user ->
            match user.original_role with
            | Insomniac -> not (List.length user.inputs = 2)
            | Drunk _ -> not (List.length user.inputs = 2)
            | Doppelganger (Some Insomniac) -> not (List.length user.inputs = 4)
            | _ -> false )
        |> Hashtbl.is_empty
      in
      if phase_over then
        let open Async in
        don't_wait_for
          (let%map () =
             Clock.after (Time.Span.of_int_sec (2 + Random.int 2))
           in
           t.phase <- Day ;
           maybe_change_phase t)

let on_input t username input =
  match Hashtbl.find t.users username with
  | None -> Or_error.error_string "Unknown user"
  | Some user ->
      if validate_input t user input then (
        record_action t user input ;
        user.inputs <- input :: user.inputs ;
        ( match t.phase with
        | Night _ -> (
            let original_role =
              match user.original_role with
              | Doppelganger (Some role) -> role
              | role -> role
            in
            match original_role with
            | Robber _ -> (
              match input with
              | Ack -> ()
              | Choose_user [username] -> (
                match Hashtbl.find t.users username with
                | None -> assert false
                | Some other_user ->
                    let other_role = other_user.current_role in
                    other_user.current_role <- user.current_role ;
                    user.current_role <- other_role )
              | _ -> assert false )
            | Voodoo_lou _ -> (
              match input with
              | Ack -> ()
              | Choose_user [username] ->
                  let chosen_user = Hashtbl.find_exn t.users username in
                  let current_role = chosen_user.current_role in
                  let center_card_idx =
                    Random_cache.voodoo_lou_card t.random_cache user.username
                  in
                  let center_card =
                    List.nth_exn t.center_cards center_card_idx
                  in
                  chosen_user.current_role <- center_card ;
                  t.center_cards
                  <- List.mapi t.center_cards ~f:(fun idx card ->
                         if idx = center_card_idx then current_role else card
                     )
              | _ -> () )
            | Troublemaker _ -> (
              match input with
              | Ack -> ()
              | Choose_user [user1; user2] -> (
                match
                  (Hashtbl.find t.users user1, Hashtbl.find t.users user2)
                with
                | None, _ | _, None -> assert false
                | Some user1, Some user2 ->
                    let role1 = user1.current_role in
                    user1.current_role <- user2.current_role ;
                    user2.current_role <- role1 )
              | _ -> assert false )
            | Alpha_wolf -> (
              match input with
              | Ack -> ()
              | Choose_user [username] ->
                  let other_user = Hashtbl.find_exn t.users username in
                  let center_role = t.center_werewolf_card in
                  t.center_werewolf_card <- other_user.current_role ;
                  other_user.current_role <- center_role
              | _ -> assert false )
            | Doppelganger None -> (
              match input with
              | Ack -> ()
              | Choose_user [username] ->
                  let other_user = Hashtbl.find_exn t.users username in
                  user.original_role
                  <- Doppelganger (Some other_user.original_role) ;
                  user.current_role
                  <- Doppelganger (Some other_user.original_role)
              | _ -> assert false )
            | Doppelganger _ -> ()
            | Werewolf | Mystic_wolf | Dream_wolf -> ()
            | Mason -> ()
            | Seer -> ()
            | Insomniac -> ()
            | Villager -> ()
            | Minion -> ()
            | Drunk _ -> ()
            | Tanner -> ()
            | Hunter -> () )
        | _ -> () ) ;
        maybe_change_phase t ; Ok () )
      else Or_error.error_string "Invalid input"

let get_page t username =
  match Hashtbl.find t.users username with
  | None -> [Page.Element.Text "Page not found"]
  | Some user -> get_page_for_user t user
