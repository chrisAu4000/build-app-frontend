module Adress.Model exposing (..)

import Data.ValidationInput as ValidationInput exposing (ValidationInput, pure)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode


type alias Street =
    ValidationInput String


type alias HouseNr =
    ValidationInput String


type alias PostCode =
    ValidationInput String


type alias Domicile =
    ValidationInput String


type alias Adress =
    { street : Street
    , houseNr : HouseNr
    , postCode : PostCode
    , domicile : Domicile
    }


empty : Adress
empty =
    { street = ValidationInput.Ok ""
    , houseNr = ValidationInput.Ok ""
    , postCode = ValidationInput.Ok ""
    , domicile = ValidationInput.Ok ""
    }


encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe f mb =
    case mb of
        Nothing ->
            Encode.null

        Just a ->
            f a


adressEncoder : Adress -> Encode.Value
adressEncoder adress =
    let
        attributes =
            [ ( "street", Encode.string (ValidationInput.get adress.street) )
            , ( "houseNr", Encode.string (ValidationInput.get adress.houseNr) )
            , ( "postCode", Encode.string (ValidationInput.get adress.postCode) )
            , ( "domicile", Encode.string (ValidationInput.get adress.domicile) )
            ]
    in
    Encode.object attributes


adressDecoder : Decode.Decoder Adress
adressDecoder =
    decode (\street houseNr postCode domicile -> Adress (pure street) (pure houseNr) (pure postCode) (pure domicile))
        |> required "street" Decode.string
        |> required "houseNr" Decode.string
        |> required "postCode" Decode.string
        |> required "domicile" Decode.string
