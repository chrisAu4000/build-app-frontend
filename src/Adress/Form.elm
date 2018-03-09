module Adress.Form exposing (..)
import Adress.Model exposing (Adress, Street, HouseNr, PostCode, Domicile)
import Adress.Validation exposing (validateStreet, validateHouseNr, validatePostCode, validateDomicile)
import Html exposing (Html, div, form)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput)
import Component.Form exposing (labeledTextInput)
import Validation exposing (Validation, (<*>), toBool)

type alias Model =
  { street : Street
  , streetError : Validation (List String) Street
  , houseNr : HouseNr
  , houseNrError : Validation (List String) HouseNr
  , postCode : PostCode
  , postCodeError : Validation (List String) PostCode
  , domicile : Domicile
  , domicileError : Validation (List String) Domicile
  , isValid : Bool
  }

init : Model
init =
  { street = ""
  , streetError = Validation.Err []
  , houseNr = ""
  , houseNrError = Validation.Err []
  , postCode = ""
  , postCodeError = Validation.Err []
  , domicile = ""
  , domicileError = Validation.Err []
  , isValid = False
  }

type Msg
  = InputStreet Street
  | InputHouseNr HouseNr
  | InputPostCode PostCode
  | InputDomicile Domicile

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputStreet street ->
      let 
        newModel = 
          { model
          | street = street
          , streetError = validateStreet street
          }
      in
        ({ newModel | isValid = validateAdressForm newModel |> toBool }, Cmd.none)
    InputHouseNr houseNr ->
      let 
        newModel = 
          { model
          | houseNr = houseNr
          , houseNrError = validateHouseNr houseNr
          }
      in
        ({ newModel | isValid = validateAdressForm newModel |> toBool }, Cmd.none)
    InputPostCode postCode ->
      let 
        newModel = 
          { model
          | postCode = postCode
          , postCodeError = validatePostCode postCode
          }
      in
        ({ newModel | isValid = validateAdressForm newModel |> toBool }, Cmd.none)
    InputDomicile domicile ->
       let 
        newModel = 
          { model
          | domicile = domicile
          , domicileError = validateDomicile domicile
          }
      in
        ({ newModel | isValid = validateAdressForm newModel |> toBool }, Cmd.none)

view : Model -> Html Msg
view model =
  form
    [ class "company-adress-form" ]
    [ div
      [ class "flex" ]
      [ div
        [class "col-8 pr1"]
        [ labeledTextInput "Street"
          [ value model.street
          , onInput InputStreet
          ]
          []
        ]
      , div
        [ class "col-4 pl1" ]
        [ labeledTextInput "HouseNr."
          [ value model.houseNr
          , onInput InputHouseNr
          ]
          []
        ]
      ]
    , div
      [ class "flex" ]
      [ div
        [ class "col-4 pr1" ]
        [ labeledTextInput "Post Code"
          [ value model.postCode
          , onInput InputPostCode
          ]
          []
        ]
      , div
        [ class "col-8 pl1" ]
        [ labeledTextInput "Domicile" 
          [ value model.domicile
          , onInput InputDomicile
          ]
          []
        ]
      ]
    ]

validateAdressForm : Model -> Validation (List String) Adress
validateAdressForm model =
  Validation.pure Adress
    <*> model.streetError
    <*> model.houseNrError
    <*> model.postCodeError
    <*> model.domicileError