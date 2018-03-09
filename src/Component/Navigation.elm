module Component.Navigation exposing (navDefault, navBack, publicNavigation, privateNavigation)

import Html exposing (Attribute, Html, a, div, i, nav, span, text)
import Html.Attributes exposing (class)
import User.Model exposing (User)

type alias Headline = String

headline : Headline -> Html msg
headline h1 =
  span [ class "navbar-headline" ] [ text h1 ]

headlineBtn : Headline -> List (Attribute msg) -> Html msg
headlineBtn h1 attrs =
  a 
    ([ class "btn regular navbar-headline" ] ++ attrs)
    [ i [ class "fa fa-chevron-left mr1" ] [], text h1 ]

navigation : List (Html msg) -> Html msg
navigation children =
  nav
    [ class "navbar clearfix white bg-blue flex" ]
    [ div 
      [class "ml2 align-v-middle"] 
      children
    ]

navDefault : (List (Html msg)) -> Html msg
navDefault h1 = 
  navigation h1

navBack : Headline -> List (Attribute msg) -> Html msg
navBack h1 attrs =
  navigation [ headlineBtn h1 attrs ]

publicNavigation : Html msg -> Html msg
publicNavigation children =
  div
    []
    [ navDefault [ text "BuildApp" ]
    , children
    ]

privateNavigation : Maybe User -> Html msg -> Html msg
privateNavigation user children =
  div
    []
    [ navDefault [ text "BuildApp private" ]
    , children
    ]
