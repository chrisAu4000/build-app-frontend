module User.Registration exposing (Model, Msg, init, update, view)

import Auth.Model exposing (Auth)
import Component.Button
    exposing
        ( WebButton(..)
        , submitBtnFailure
        , submitBtnLoading
        , submitBtnNotAsked
        , submitBtnSuccess
        )
import Component.Form exposing (centeredForm)
import Component.Input as Input exposing (labeledPasswordInputValidation, labeledTextInputValidation)
import Data.ValidationInput as ValidationInput exposing ((<*>), ValidationInput)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import RemoteData exposing (WebData)
import User.Model exposing (Email, Password, User, Username)
import User.Request exposing (RegistrationData, registerUser)
import User.Validation exposing (validateEmail, validatePassword, validateUsername, validateVerification)


type alias UsernameInput =
    ValidationInput String


type alias EmailInput =
    ValidationInput String


type alias PasswordInput =
    ValidationInput String


type alias VerificationInput =
    ValidationInput String


type alias Model =
    { username : UsernameInput
    , email : EmailInput
    , password : PasswordInput
    , verification : VerificationInput
    , btnState : WebButton String
    }


type Msg
    = InputUsername Username
    | InputEmail Email
    | InputPassword Password
    | InputPasswordVerification String
    | Submit UsernameInput EmailInput PasswordInput VerificationInput
    | OnRegisteredUser (WebData Auth)


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { username = ValidationInput.Ok ""
            , email = ValidationInput.Ok ""
            , password = ValidationInput.Ok ""
            , verification = ValidationInput.Ok ""
            , btnState = btnStateNotAsked
            }
    in
    ( initialModel, Cmd.none )


btnStateNotAsked : WebButton String
btnStateNotAsked =
    NotAsked "Register"


btnStateLoading : WebButton String
btnStateLoading =
    Loading ""


btnStateNetworkError : WebButton String
btnStateNetworkError =
    Failure "Sorry... Registration Failed"


btnStateValidationError : WebButton String
btnStateValidationError =
    Failure "Validation caught you?"


btnStateSuccess : WebButton String
btnStateSuccess =
    Success "Check your Mailbox"


validateRegistration :
    UsernameInput
    -> EmailInput
    -> PasswordInput
    -> VerificationInput
    -> ValidationInput RegistrationData
validateRegistration username email password verification =
    ValidationInput.pure RegistrationData
        <*> validateUsername username
        <*> validateEmail email
        <*> validatePassword password
        <*> validateVerification verification password


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputUsername username ->
            ( { model
                | username = ValidationInput.Ok username
                , btnState = btnStateNotAsked
              }
            , Cmd.none
            )

        InputEmail email ->
            ( { model
                | email = ValidationInput.Ok email
                , btnState = btnStateNotAsked
              }
            , Cmd.none
            )

        InputPassword password ->
            ( { model
                | password = ValidationInput.Ok password
                , btnState = btnStateNotAsked
              }
            , Cmd.none
            )

        InputPasswordVerification verification ->
            ( { model
                | verification = ValidationInput.Ok verification
                , btnState = btnStateNotAsked
              }
            , Cmd.none
            )

        Submit username email password verification ->
            case validateRegistration username email password verification of
                ValidationInput.Err msgs val ->
                    ( { model
                        | btnState = btnStateValidationError
                        , username = validateUsername username
                        , email = validateEmail email
                        , password = validatePassword password
                        , verification = validateVerification verification password
                      }
                    , Cmd.none
                    )

                ValidationInput.Ok data ->
                    ( { model | btnState = btnStateLoading }, registerUser data |> Cmd.map OnRegisteredUser )

        OnRegisteredUser wdUser ->
            case wdUser of
                RemoteData.Failure _ ->
                    ( { model | btnState = btnStateNetworkError }, Cmd.none )

                RemoteData.Success user ->
                    ( { model | btnState = btnStateSuccess }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        submitBtn =
            case model.btnState of
                NotAsked s ->
                    submitBtnNotAsked s

                Loading s ->
                    submitBtnLoading s

                Failure s ->
                    submitBtnFailure s

                Success s ->
                    submitBtnSuccess s
    in
    div
        [ class "login-form flex-column" ]
        [ div
            [ class "flex" ]
            [ centeredForm
                [ onSubmit (Submit model.username model.email model.password model.verification) ]
                [ labeledTextInputValidation
                    InputUsername
                    "Username"
                    model.username
                , labeledTextInputValidation
                    InputEmail
                    "Email"
                    model.email
                , labeledPasswordInputValidation
                    InputPassword
                    "Password"
                    model.password
                , labeledPasswordInputValidation
                    InputPasswordVerification
                    "Verifiy Password"
                    model.verification
                , submitBtn
                ]
            ]
        ]
