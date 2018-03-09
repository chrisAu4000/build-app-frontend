module Component.List exposing (listHeader)

import Html exposing (Html, div, a, i)
import Html.Attributes exposing (class, href)
import Component.Icon exposing (Icon, icon)

listHeaderBtn : (Icon, String) -> Html msg
listHeaderBtn (icn, url) =
  div 
    [ class "col col-4 pr2" ] 
    [ a 
      [ class "btn regular h2" 
      , href url
      ] 
      [ i [ class (icon icn) ] [] ] 
    ]

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