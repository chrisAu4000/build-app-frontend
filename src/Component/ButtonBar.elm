module Component.ButtonBar exposing (..)

import Html exposing (Attribute, Html, button, div, i)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Ability msg
    = Enabled msg
    | Disabled


type Visibility msg
    = Visible (Ability msg)
    | Hidden


type alias ButtonL msg =
    Visibility msg


type alias ButtonR msg =
    Visibility msg


wizardButtons : ( ButtonL msg, ButtonR msg ) -> Html msg
wizardButtons ( bl, br ) =
    let
        toClasses : Visibility msg -> ( String, List (Attribute msg) )
        toClasses b =
            case b of
                Hidden ->
                    ( " hidden disabled", [] )

                Visible bv ->
                    case bv of
                        Disabled ->
                            ( " disabled", [] )

                        Enabled msg ->
                            ( "", [ onClick msg ] )

        ( leftButtonClasses, leftButtonAction ) =
            toClasses bl

        ( rightButtonClasses, rightButtonAction ) =
            toClasses br
    in
    div
        [ class "wizard-button-bar flex relative height-0" ]
        [ button
            ([ class ("btn circle wizard-button blue" ++ leftButtonClasses) ] ++ leftButtonAction)
            [ i
                [ class "icon fa fa-arrow-circle-o-left fa-4x" ]
                []
            ]
        , button
            ([ class ("btn circle wizard-button right green" ++ rightButtonClasses) ] ++ rightButtonAction)
            [ i
                [ class "icon fa fa-check-circle-o fa-4x" ]
                []
            ]
        ]
