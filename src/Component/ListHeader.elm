module Component.ListHeader exposing (..)

import Html exposing (Html, div, a, i)
import Html.Attributes exposing (class, href)
import Component.Button exposing (listHeaderButton)
import Component.Icon exposing (Icon, icon)


listHeader : Maybe ( List (Icon, String) ) -> Html msg
listHeader mbUrl =
  case mbUrl of
    Just urls -> 
      div [ class "subnav clearfix" ] 
        (List.map listHeaderBtn urls)
    Nothing -> 
      div 
        [ class "subnav clearfix" ]
        []