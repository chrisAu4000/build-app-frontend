module Component.Form exposing (..)

import Html exposing (Attribute, Html, div, form, input, label, li, text, textarea, ul)
import Html.Attributes exposing (class, for, id, required, style, type_, value)


type alias Label =
    String


centeredForm : List (Attribute msg) -> List (Html msg) -> Html msg
centeredForm attrs children =
    let
        attributes =
            attrs ++ [ class "clearfix m1 form" ]
    in
    div
        [ class "clearfix align-h-middle col sm-col-12 md-col-9 lg-col-6" ]
        [ form attributes children ]
