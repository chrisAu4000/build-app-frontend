module Component.Icon exposing (Icon(..), icon)

type Icon
  = Burger
  | Check
  | Delete
  | Edit
  | Failure
  | Loading
  | Success
  | None
  | Plus
  | X

icon : Icon -> String
icon icn =
  case icn of
    Burger -> "fa fa-bars"
    Check -> "fa fa-check mr1"
    Delete -> "fa fa-trash mr1"
    Edit -> "fa fa-pencil mr1"
    Failure -> "fa fa-meh-o mr1"
    Loading -> "fa fa-spinner fa-spin mr1"
    Success -> "fa fa-smile-o"
    None -> ""
    Plus -> "fa fa-plus white"
    X -> "fa fa-close"