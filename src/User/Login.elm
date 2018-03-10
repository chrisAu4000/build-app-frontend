module User.Login exposing (..)

import Auth.Model exposing (Auth)
import Component.Button exposing 
  ( submitBtnNotAsked
  , submitBtnLoading
  , submitBtnFailure
  , submitBtnSuccess
  , WebButton(..)
  )
import Component.Form exposing (centeredForm, labeledTextInputValidation, labeledPasswordInput)
import Html exposing (Html, a, div, h1, li, p, ul, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onInput, onSubmit)
import Http
import User.Model exposing (User, Username, Email, Password)
import User.Request exposing (loginUser)
import User.Validation exposing (validateEmail)
import RemoteData exposing (WebData)
import Validation exposing (Validation)

type alias Model =
  { email : Email
  , emailError : Validation (List String) String
  , password : Password
  , passwordError : Validation (List String) String
  , btnState : WebButton String
  }

type Msg
  = InputEmail Email
  | InputPassword Password
  | Submit Username Password
  | OnLoggedInUser (WebData Auth)

type RootMsg
  = None
  | SetUser Auth

init : (Model, Cmd Msg)
init =
  (
    { email = ""
    , emailError = Validation.Res ""
    , password = ""
    , passwordError = Validation.Err []
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

update : Msg -> Model -> (Model, Cmd Msg, RootMsg)
update msg model =
  case msg of
    InputEmail email ->
      ( { model | email = email }, Cmd.none, None )
    InputPassword password ->
      ( { model | password = password }, Cmd.none, None )
    Submit email password ->
      let
        emErr = validateEmail email
        user = { email = email, password = password}
      in
        case emErr of
          Validation.Err _ -> ( {model | emailError = emErr}, Cmd.none, None)
          Validation.Res _ -> ( model, loginUser user |> Cmd.map OnLoggedInUser, None)
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

-- labeledTextInputValidation model.usernameError "Username" 
--             [ value model.username
--             , onInput InputUsername 
--             ]
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
          [ labeledTextInputValidation model.emailError "E-Mail"
            [ onInput InputEmail ]
            []
          , labeledPasswordInput "Password" [ onInput InputPassword ] []
          , submitBtn
          ]
        ]
      ]
