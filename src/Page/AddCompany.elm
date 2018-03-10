module Page.AddCompany exposing (..)

import Company.Wizard as Wizard
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class,  style)
import Auth.Model exposing (Auth)

type alias Model =
  { auth : Auth
  , wizard : Wizard.Model
  }

type Msg
  = WizardMsg Wizard.Msg


init : Auth -> (Model, Cmd Msg)
init auth =
  ( { auth = auth
    , wizard = Wizard.init
    }
    , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WizardMsg wizardMsg ->
      let
        (wizard, wizMsg) = Wizard.update wizardMsg model.wizard model.auth.jwt
      in  
        ({ model | wizard = wizard }, Cmd.map WizardMsg wizMsg)

view : Model -> Html Msg
view model =
  div
   []
    [ div
      [ class "page m2 flex"]
      [ Wizard.view model.wizard |> Html.map WizardMsg ]
    ]