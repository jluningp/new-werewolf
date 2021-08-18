open Core
module Username = String

module Role = struct
  type t = Robber | Werewolf
end

module Page = struct
  module Element = struct
    type t =
      | Text of string
      | Cards of Role.t list
      | Choose_user of { choose_this_many : int; users : Username.t list }
      | Choose_role of (Role.t * int) list
  end

  type t = Element.t list
end

module Input = struct
  type t =
    | Ack
    | Choose_user of Username.t list
    | Choose_role of { role : Role.t; count : int }
end
