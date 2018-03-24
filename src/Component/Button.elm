module Component.Button exposing (..)

import Component.Icon as Icon exposing (Icon, icon)
import Html exposing (Attribute, Html, a, button, div, i, span, text)
import Html.Attributes exposing (class, href, type_)
import Html.Events exposing (onClick)


type alias ButtonLabel =
    String


btn : List (Attribute msg) -> ButtonLabel -> Html msg
btn attrs label =
    a
        ([ class "btn not-rounded col col-12 p2 white center" ] ++ attrs)
        [ text label ]


iconBtn : Icon -> List (Attribute msg) -> ButtonLabel -> Html msg
iconBtn icn attrs label =
    button
        ([ class "btn btn-submit not-rounded col col-12 p2 white " ] ++ attrs)
        [ i [ class (icon icn) ] []
        , text label
        ]


menuBtn : String -> msg -> Html msg
menuBtn cls msg =
    button
        [ class ("btn btn-regular fa-2x " ++ cls)
        , onClick msg
        ]
        [ i [ class (icon Icon.Burger) ] [] ]


closeBtn : String -> msg -> Html msg
closeBtn cls msg =
    button
        [ class ("btn btn-regular fa-2x " ++ cls)
        , onClick msg
        ]
        [ i [ class (icon Icon.X) ] [] ]


type alias Url =
    String


editBtn : Url -> Html msg
editBtn url =
    a
        [ class "btn regular px0 mx-auto"
        , href url
        ]
        [ i [ class (icon Icon.Edit) ] [], text "Edit" ]


deleteBtn : List (Attribute msg) -> Html msg
deleteBtn attrs =
    button
        ([ class "btn regular px0 mx-auto" ] ++ attrs)
        [ i [ class (icon Icon.Delete) ] [], text "Remove" ]


successBtn : String -> ButtonLabel -> Html msg
successBtn link =
    btn
        [ class "bg-green"
        , href link
        ]


listHeaderBtn : ( Icon, String ) -> Html msg
listHeaderBtn ( icn, url ) =
    a
        [ class "header-btn btn regular border"
        , href url
        ]
        [ span []
            [ i [ class ("mr1 " ++ icon icn) ] []
            , text "Add a Company"
            ]
        ]


type WebButton a
    = NotAsked a
    | Loading a
    | Failure a
    | Success a


submitBtnNotAsked : ButtonLabel -> Html msg
submitBtnNotAsked =
    iconBtn Icon.None [ type_ "submit", class "bg-blue" ]


submitBtnLoading : ButtonLabel -> Html msg
submitBtnLoading =
    iconBtn Icon.Loading [ class "disabled bg-blue" ]


submitBtnFailure : ButtonLabel -> Html msg
submitBtnFailure =
    iconBtn Icon.Failure [ class "bg-red" ]


submitBtnSuccess : ButtonLabel -> Html msg
submitBtnSuccess =
    iconBtn Icon.Check [ class "bg-green" ]
