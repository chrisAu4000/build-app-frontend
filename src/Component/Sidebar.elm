module Component.Sidebar exposing (..)

import Component.Button exposing (closeBtn, menuBtn)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Tuple


type alias IsOpen =
    Bool


sidebar : IsOpen -> ( msg, msg ) -> List (Html msg) -> Html msg
sidebar isOpen msgT children =
    if isOpen then
        div
            [ class "sidebar open clearfix left col col-3" ]
            ([ closeBtn "sidebar-btn white" (Tuple.second msgT) ] ++ children)
    else
        div
            [ class "sidebar clearfix left col col-0 ml0" ]
            ([ menuBtn "sidebar-btn white" (Tuple.first msgT) ] ++ children)
