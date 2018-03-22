module User.Login exposing (..)

import Auth.Model exposing (Auth)
import Component.Button exposing 
  ( submitBtnNotAsked
  , submitBtnLoading
  , submitBtnFailure
  , submitBtnSuccess
  , WebButton(..)
  )
import Component.Form exposing (centeredForm)
import Component.Input exposing (labeledTextInputValidation, labeledPasswordInputValidation)
import Data.ValidationInput as ValidationInput exposing (ValidationInput, (<*>))
import Html exposing (Html, a, div, h1, li, p, ul, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onInput, onSubmit)
import Http
import User.Model exposing (User, Username, Email, Password)
import User.Request exposing (LoginData, loginUser)
import User.Validation exposing (validateEmail, validatePassword)
import RemoteData exposing (WebData)

type alias EmailInput = ValidationInput String
type alias PasswordInput = ValidationInput String

type alias Model =
  { email : EmailInput
  , password : PasswordInput
  , btnState : WebButton String
  }

type Msg
  = InputEmail Email
  | InputPassword Password
  | Submit EmailInput PasswordInput
  | OnLoggedInUser (WebData Auth)

type RootMsg
  = None
  | SetUser Auth

init : (Model, Cmd Msg)
init =
  (
    { email = ValidationInput.Ok ""
    , password = ValidationInput.Ok ""
    , btnState = btnStateNotAsked
    }
  , Cmd.none
  )

parseHttpError : Http.Error -> String
parseHttpError error =
  case error of
    Http.BadStatus detail -> "Username or Password doesn't match."
    Http.BadUrl _ -> "Oh Oh... Someting went wrong"
    Http.BadPayload _ _ -> "Bad Payload"
    Http.Timeout -> "Request took to long"
    Http.NetworkError -> "Connected to the internet?"

btnStateNotAsked : WebButton String
btnStateNotAsked = NotAsked "Login"

btnStateLoading : WebButton String
btnStateLoading = Loading ""

btnStateFailure : String -> WebButton String
btnStateFailure = Failure

btnStateSuccess : WebButton String
btnStateSuccess = Success "Yey Saved Position"

validateLogin : EmailInput -> PasswordInput -> ValidationInput LoginData
validateLogin email password =
  ValidationInput.pure LoginData
  <*> validateEmail (ValidationInput.get email)
  <*> validatePassword (ValidationInput.get password)

update : Msg -> Model -> (Model, Cmd Msg, RootMsg)
update msg model =
  case msg of
    InputEmail email ->
      ( { model | email = ValidationInput.Ok email }, Cmd.none, None )
    InputPassword password ->
      ( { model | password = ValidationInput.Ok password }, Cmd.none, None )
    Submit email password ->
      case validateLogin email password of
        ValidationInput.Err errs _ -> 
          ( { model 
            | email = validateEmail (ValidationInput.get email)
            , password = validatePassword (ValidationInput.get password)
            }
          , Cmd.none, None
          )
        ValidationInput.Ok user -> 
          ( model, loginUser user |> Cmd.map OnLoggedInUser, None)
    OnLoggedInUser webUser ->
      case webUser of
        RemoteData.NotAsked ->
          ( { model | btnState = btnStateNotAsked }, Cmd.none, None )
        RemoteData.Loading ->
          ( { model | btnState = btnStateLoading }, Cmd.none, None )
        RemoteData.Failure error ->
          let
            errorMessage =
              parseHttpError error
          in
            ( { model | btnState = btnStateFailure errorMessage }
            , Cmd.none
            , None
            )
        RemoteData.Success user -> ( model, Cmd.none, SetUser user )

view : Model -> Html Msg
view model =
  let
    submitBtn =
      case model.btnState of
        NotAsked s -> submitBtnNotAsked s
        Loading s -> submitBtnLoading s
        Failure s -> submitBtnFailure s
        Success s -> submitBtnSuccess s
  in
    div
      [ class "login-form flex-column" ]
      [ div
        [ class "flex" ]
        [ centeredForm
          [ onSubmit (Submit model.email model.password) ]
          [ labeledTextInputValidation
            InputEmail "E-Mail" model.email
          , labeledPasswordInputValidation 
            InputPassword "Password" model.password
          , submitBtn
          ]
        ]
      ]
