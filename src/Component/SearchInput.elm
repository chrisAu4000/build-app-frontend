module Component.SearchInput exposing (..)

import Html exposing (Html, div, i, input)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick, onInput)


searchInput : (String -> msg) -> Html msg
searchInput oninput =
    div
        [ class "search-input ml1" ]
        [ input
            [ class "input not-rounded search-input--input "
            , type_ "text"
            , onInput oninput
            ]
            []
        , i
            [ class "search-input--icon fa fa-2x fa-search" ]
            []
        ]
