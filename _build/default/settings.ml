type t =
  { mutable lone_werewolf_sees_non_werewolf_center_card : bool
  ; mutable show_role_number : bool }

let copy t =
  { lone_werewolf_sees_non_werewolf_center_card =
      t.lone_werewolf_sees_non_werewolf_center_card
  ; show_role_number = t.show_role_number }
