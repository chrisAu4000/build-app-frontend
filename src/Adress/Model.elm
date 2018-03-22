module Adress.Model exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Data.Validation exposing (Validation)

type alias Street = String
type alias HouseNr = String
type alias PostCode = String
type alias Domicile = String

type alias Adress =
  { street : Street
  , houseNr : HouseNr
  , postCode : PostCode
  , domicile : Domicile
  }

type alias ValidAdress = Validation (List String) Adress

empty : Adress
empty =
  { street = ""
  , houseNr = ""
  , postCode = ""
  , domicile = ""
  }

encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe f mb =
  case mb of
    Nothing -> Encode.null
    Just a -> f a

adressEncoder : Adress -> Encode.Value
adressEncoder adress =
  let
    attributes =
      [ ( "street", Encode.string adress.street )
      , ( "houseNr", Encode.string adress.houseNr )
      , ( "postCode", Encode.string adress.postCode )
      , ( "domicile", Encode.string adress.domicile)
      ]
  in
    Encode.object attributes

adressDecoder : Decode.Decoder Adress
adressDecoder =
  decode Adress
    |> required "street" Decode.string
    |> required "houseNr" Decode.string
    |> required "postCode" Decode.string
    |> required "domicile" Decode.string