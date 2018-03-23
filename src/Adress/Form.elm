module Adress.Form exposing (..)

import Adress.Model exposing (Adress)
import Component.Input exposing (labeledTextInput, labeledTextInputValidation)
import Data.ValidationInput as ValidationInput exposing (ValidationInput)
import Html exposing (Html, div, form)
import Html.Attributes exposing (class, value)


init : Adress
init =
    { street = ValidationInput.Ok ""
    , houseNr = ValidationInput.Ok ""
    , postCode = ValidationInput.Ok ""
    , domicile = ValidationInput.Ok ""
    }


type Msg
    = InputStreet String
    | InputHouseNr String
    | InputPostCode String
    | InputDomicile String


update : Msg -> Adress -> ( Adress, Cmd Msg )
update msg model =
    case msg of
        InputStreet street ->
            ( { model | street = ValidationInput.Ok street }, Cmd.none )

        InputHouseNr houseNr ->
            ( { model | houseNr = ValidationInput.Ok houseNr }, Cmd.none )

        InputPostCode postCode ->
            ( { model | postCode = ValidationInput.Ok postCode }, Cmd.none )

        InputDomicile domicile ->
            ( { model | domicile = ValidationInput.Ok domicile }, Cmd.none )


view : Adress -> Html Msg
view model =
    form
        [ class "company-adress-form mb1" ]
        [ div
            [ class "flex" ]
            [ div
                [ class "col-8 pr1" ]
                [ labeledTextInputValidation
                    InputStreet
                    "Street"
                    model.street
                ]
            , div
                [ class "col-4 pl1" ]
                [ labeledTextInputValidation
                    InputHouseNr
                    "HouseNr."
                    model.houseNr
                ]
            ]
        , div
            [ class "flex" ]
            [ div
                [ class "col-4 pr1" ]
                [ labeledTextInputValidation
                    InputPostCode
                    "Post Code"
                    model.postCode
                ]
            , div
                [ class "col-8 pl1" ]
                [ labeledTextInputValidation
                    InputDomicile
                    "Domicile"
                    model.domicile
                ]
            ]
        ]
