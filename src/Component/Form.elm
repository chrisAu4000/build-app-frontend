module Component.Form exposing (..)

import Html exposing (Attribute, Html, div, input, label, li, form, text, textarea, ul)
import Html.Attributes exposing (class, id, for, type_, required, style, value)
import Validation exposing (Validation)

type alias Label = String

centeredForm : List (Attribute msg) -> List (Html msg) -> Html msg
centeredForm attrs children =
  let
    attributes = attrs ++ [ class "clearfix m1 form" ]
  in
    div 
      [ class "clearfix align-h-middle col col-6" ]
      [ form attributes children ]

