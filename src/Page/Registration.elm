module Page.Registration exposing (..)

import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)
import User.Registration as Registration


type alias Model =
    { registration : Registration.Model }


type Msg
    = RegistrationMsg Registration.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( model, msg ) =
            Registration.init
    in
    ( { registration = model }, Cmd.map RegistrationMsg msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RegistrationMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    Registration.update subMsg model.registration
            in
            ( { model | registration = subModel }, Cmd.map RegistrationMsg subCmd )


view : Model -> Html Msg
view model =
    let
        registrationForm =
            Registration.view model.registration
                |> Html.map RegistrationMsg
    in
    div
        [ class "clearfix page flex flex-column login-page" ]
        [ h1
            [ class "ml2" ]
            [ text "Registration" ]
        , registrationForm
        , div
            [ class "mt2 align-h-middle" ]
            [ text "Already registered? Login "
            , a [ href "#/login" ] [ text " here" ]
            ]

        -- , text (toString model.login)
        ]
