module Company.Form exposing (..)

import Company.Validation exposing (validateCompanyName)
import Component.Form exposing (labeledTextInput)
import Html exposing (Html, form, input)
import Html.Attributes exposing (class, value, type_)
import Html.Events exposing (onInput)

import Validation exposing (Validation, (<*>), toBool)
type alias Name = String

type alias Model =
  { name : Name
  , nameError : Validation (List String) Name
  , isValid : Bool
  }

type Msg
  = InputCompanyName Name

init : Model
init =
  { name = ""
  , nameError = Validation.Err []
  , isValid = False
  }

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    InputCompanyName name ->
      let
        newModel = { model | name = name, nameError = validateCompanyName name }
      in
      ( { newModel | isValid = validateCompanyForm newModel |> toBool }, Cmd.none )

view : Model -> Html Msg
view model =
  form
    [ class "company-name-form" ]
    [ labeledTextInput "Company Name"
      [ value model.name
      , onInput InputCompanyName
      ]
      []
    ]

validateCompanyForm : Model -> Validation (List String) Name
validateCompanyForm model = model.nameError
