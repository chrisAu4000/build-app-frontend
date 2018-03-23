module Component.ListHeader exposing (..)

import Component.Button exposing (listHeaderButton)
import Component.Icon exposing (Icon, icon)
import Html exposing (Html, a, div, i)
import Html.Attributes exposing (class, href)


listHeader : Maybe (List ( Icon, String )) -> Html msg
listHeader mbUrl =
    case mbUrl of
        Just urls ->
            div [ class "subnav clearfix" ]
                (List.map listHeaderBtn urls)

        Nothing ->
            div
                [ class "subnav clearfix" ]
                []
