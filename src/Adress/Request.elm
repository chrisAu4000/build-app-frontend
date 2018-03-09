module Adress.Request exposing (adressEncoder, adressDecoder)

import Adress.Model exposing (Adress)
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)

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
      , ( "houseNumber", Encode.string adress.houseNr )
      , ( "postCode", Encode.string adress.postCode )
      , ( "domicile", Encode.string adress.domicile)
      ]
  in
    Encode.object attributes

adressDecoder : Decode.Decoder Adress
adressDecoder =
  decode Adress
    |> required "street" Decode.string
    |> required "houseNumber" Decode.string
    |> required "postCode" Decode.string
    |> required "domicile" Decode.string