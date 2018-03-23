module Component.Completed exposing (CompleteMsg(..), view)

import Component.Button exposing (submitBtnNotAsked)
import Component.Icon as Icon exposing (icon)
import Html exposing (Html, div, form, i, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import RemoteData exposing (WebData)


type CompleteMsg msg
    = CompleteMsg msg


view : ( CompleteMsg msg, CompleteMsg msg ) -> WebData a -> Html msg
view ( onRetry, onReset ) company =
    let
        retry =
            case onRetry of
                CompleteMsg msg ->
                    msg

        reset =
            case onReset of
                CompleteMsg msg ->
                    msg

        ( iconClass, message, button, action ) =
            case company of
                RemoteData.Loading ->
                    ( icon Icon.Loading, "Saving...", Nothing, [] )

                RemoteData.Failure e ->
                    ( "red " ++ icon Icon.Failure
                    , "Something went wrong"
                    , Just (submitBtnNotAsked "Try Again")
                    , [ onSubmit retry ]
                    )

                RemoteData.Success c ->
                    ( "green " ++ icon Icon.Success
                    , "Company Saved"
                    , Just (submitBtnNotAsked "Add another company")
                    , [ onSubmit reset ]
                    )

                RemoteData.NotAsked ->
                    ( "", "", Nothing, [] )
    in
    form
        ([ class "wizard-complete flex" ] ++ action)
        [ div
            [ class "align-v-middle align-h-middle center flex flex-column" ]
            [ i
                [ class ("fa-4x center " ++ iconClass) ]
                []
            , div
                [ class "mb1" ]
                [ text message ]
            , Maybe.withDefault (div [] []) button
            ]
        ]
