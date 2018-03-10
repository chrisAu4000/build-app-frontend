module Page.Login exposing (Model, Msg, RootMsg(..), init, update, view)

import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class,  href)
import User.Login as Login
import User.Model exposing (Auth)

type alias Model =
  { login : Login.Model }

type Msg
  = LoginMsg Login.Msg

type RootMsg
  = SetUser Auth
  | None

init : (Model, Cmd Msg)
init =
  let
    ( model, msg ) = Login.init
  in
    ( { login = model }, Cmd.map LoginMsg msg )

update : Msg -> Model -> (Model, Cmd Msg, RootMsg)
update msg model =
  case msg of
    LoginMsg subMsg ->
      let
        ( subModel, subCmd, rootMsg) =
          (Login.update subMsg model.login)
        cmd =
          case rootMsg of
            Login.None -> None
            Login.SetUser user -> SetUser user
      in
        ( { model | login = subModel }, Cmd.map LoginMsg subCmd , cmd )

view : Model -> Html Msg
view model = 
  let
    loginForm = Login.view model.login |> Html.map LoginMsg
  in
    div
      [ class "clearfix page flex flex-column login-page" ]
      [ h1
        [ class "ml2"]
        [ text "Login" ]
      , loginForm
      , div
          [ class "mt2 align-h-middle"]
          [ text "Don't have an Account? Register "
          , a [ href "#/registration" ] [ text " here"]
          ]
      ]
  
