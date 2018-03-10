module User.Registration exposing (Model, Msg, init, update, view)
import Auth.Model exposing (Auth)
import Component.Button exposing 
  ( submitBtnNotAsked
  , submitBtnLoading
  , submitBtnFailure
  , submitBtnSuccess
  , WebButton(..)
  )
import Component.Form exposing
  ( centeredForm
  , labeledTextInputValidation
  , labeledPasswordInputValidation
  )
import Debouncer.Basic as Debouncer exposing
  ( Debouncer
  , provideInput
  , settleWhenQuietFor
  , toDebouncer
  )
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onSubmit, onInput, onClick)
import User.Model exposing (User, Username, Email, Password)
import User.Request exposing (RegistrationData, registerUser)
import User.Validation exposing (validateEmail, validateUsername, validatePassword, validateVerification)
import Validation exposing (Validation, (<*>))
import RemoteData exposing (WebData)
import Time

type alias PasswordVerification = String

type alias Model =
  { username : Username
  , usernameError : Validation (List String) Username
  , email : Email
  , emailError : Validation (List String) Email
  , password : Password
  , passwordError : Validation (List String) Password
  , passwordVerification : PasswordVerification
  , verificationError : Validation (List String) Password
  , btnState : WebButton String
  , debouncer : Debouncer Msg Msg
  }

type Msg
  = InputUsername Username
  | InputEmail Email
  | InputPassword Password
  | InputPasswordVerification PasswordVerification
  | Submit Username Email Password PasswordVerification
  | OnRegisteredUser (WebData Auth)
  | Debounce (Debouncer.Msg Msg)

init : (Model, Cmd Msg)
init =
  let
    initialModel =
      { username = ""
      , usernameError = Validation.Res ""
      , email = ""
      , emailError = Validation.Res ""
      , password = ""
      , passwordError = Validation.Res ""
      , passwordVerification = ""
      , verificationError = Validation.Res ""
      , btnState = btnStateNotAsked
      , debouncer = Debouncer.debounce (1 * Time.second) |> toDebouncer
      }
  in 
    (initialModel, Cmd.none)

btnStateNotAsked : WebButton String
btnStateNotAsked = NotAsked "Register"

btnStateLoading : WebButton String
btnStateLoading = Loading ""

btnStateNetworkError : WebButton String
btnStateNetworkError = Failure "Sorry... Registration Failed"

btnStateValidationError : WebButton String
btnStateValidationError = Failure "Validation caught you?"

btnStateSuccess : WebButton String
btnStateSuccess = Success "Check your Mailbox"


validateRegistration : Username -> Email -> Password -> String -> Validation (List String) RegistrationData
validateRegistration username email password verification =
  Validation.pure RegistrationData
    <*> validateUsername username
    <*> validateEmail email
    <*> validatePassword password
    <*> validateVerification verification password

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputUsername username ->
      ( { model |
          username = username
        , usernameError = Validation.Res username
        , btnState = btnStateNotAsked
        }, Cmd.none
      )
    InputEmail email ->
      ( { model |
          email = email
        , emailError = Validation.Res email
        , btnState = btnStateNotAsked
        }
        , Cmd.none
      )
    InputPassword password ->
      ( { model |
          password = password
        , passwordError = Validation.Res password
        , btnState = btnStateNotAsked
        }
        , Cmd.none
      )
    InputPasswordVerification verification ->
      ( { model |
          passwordVerification = verification
        , verificationError = Validation.Res verification
        , btnState = btnStateNotAsked
        }
      , Cmd.none 
      )
    Submit username email password verification ->
      let
        unErr = validateUsername username
        emErr = validateEmail email
        pwErr = validatePassword password
        vfErr = validateVerification verification password
        requestData = validateRegistration username email password verification
      in
        case requestData of
          Validation.Err msgs -> 
            ( { model |
                btnState = btnStateValidationError
              , usernameError = unErr
              , emailError = emErr
              , passwordError = pwErr
              , verificationError = vfErr
              }
            , Cmd.none 
            )
          Validation.Res data ->
            ( { model | btnState = btnStateLoading }, registerUser data |> Cmd.map OnRegisteredUser )
    OnRegisteredUser wdUser ->
      let
        _ = Debug.log "register:" (toString wdUser)
      in
        case wdUser of
          RemoteData.Failure _ ->
            ( { model | btnState = btnStateNetworkError }, Cmd.none )
          RemoteData.Success user ->
            ( { model | btnState = btnStateSuccess }, Cmd.none )
          _ -> ( model, Cmd.none )
    Debounce debMsg ->
      let
        ( subModel, subCmd, emittedMsg ) =
          Debouncer.update debMsg model.debouncer
        mappedCmd =
          Cmd.map Debounce subCmd
        updatedModel =
          { model | debouncer = subModel }
      in
        case emittedMsg of
          Just emitted ->
            update emitted updatedModel
              |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, mappedCmd ])
          Nothing ->
            ( updatedModel, mappedCmd )

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
          [ onSubmit (Submit model.username model.email model.password model.passwordVerification) ]
          [ labeledTextInputValidation model.usernameError "Username" 
            [ value model.username
            , onInput InputUsername 
            ]
            []
          , labeledTextInputValidation model.emailError "Email"
            [ value model.email
            , onInput InputEmail
            ]
            []
          , labeledPasswordInputValidation model.passwordError "Password"
            [ value model.password
            , onInput InputPassword
            ]
            []
          , labeledPasswordInputValidation model.verificationError "Verifiy Password" 
            [ value model.passwordVerification
            , onInput InputPasswordVerification 
            ]
            []
          , submitBtn
          ]
        ]
      ]
