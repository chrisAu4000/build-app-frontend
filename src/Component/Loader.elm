module Component.Loader exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)


load : Html msg
load =
    div
        [ class "flex justify-center loader" ]
        [ i
            [ class "fa fa-3x fa-spinner fa-spin" ]
            []
        ]
