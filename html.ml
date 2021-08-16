open Core

module Attr = struct
  type t = string * string

  let to_string (tag, value) =
    match value with "" -> tag | _ -> sprintf "%s=\"%s\"" tag value
end

type t =
  | Div of Attr.t list * t list
  | Button of Attr.t list * t list
  | P of Attr.t list * t list
  | Label of Attr.t list * t list
  | Li of Attr.t list * t list
  | Ul of Attr.t list * t list
  | Input of Attr.t list * t list
  | Text of string
  | Br
  | Refresh_page

let div a t = Div (a, t)

let button a t = Button (a, t)

let p a t = P (a, t)

let label a t = Label (a, t)

let li a t = Li (a, t)

let ul a t = Ul (a, t)

let input a t = Input (a, t)

let text str = Text str

let br = Br

let refresh_page = Refresh_page

let attrs_to_string attrs =
  List.map attrs ~f:Attr.to_string |> String.concat ~sep:" "

let rec to_html_tags tag (a, t) =
  sprintf "<%s %s>%s</%s>" tag (attrs_to_string a)
    (List.map t ~f:to_string |> String.concat ~sep:"")
    tag

and to_string = function
  | Div (a, t) -> to_html_tags "div" (a, t)
  | Button (a, t) -> to_html_tags "button" (a, t)
  | P (a, t) -> to_html_tags "p" (a, t)
  | Label (a, t) -> to_html_tags "label" (a, t)
  | Li (a, t) -> to_html_tags "li" (a, t)
  | Ul (a, t) -> to_html_tags "ul" (a, t)
  | Input (a, t) -> to_html_tags "input" (a, t)
  | Text str -> str
  | Br -> "<br>"
  | Refresh_page ->
      to_string
        (div
           [ ("id", "refreshpage"); ("style", "display: none;") ]
           [ text "500" ])
