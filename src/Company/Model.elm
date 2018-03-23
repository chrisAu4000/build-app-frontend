module Company.Model exposing (..)

import Adress.Model exposing (Adress, Domicile, HouseNr, PostCode, Street, adressDecoder, adressEncoder)
import Data.ValidationInput as ValidationInput exposing ((<*>), ValidationInput, maxLength, minLength, pure)
import Either exposing (Either, leftToMaybe)
import Either.Decode exposing (either)
import FileReader exposing (NativeFile)
import Json.Decode as Decode exposing (maybe, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import MimeType


type alias CompanyName =
    ValidationInput String


type alias CompanyId =
    Maybe String


type alias ImageSrc =
    String


type alias Logo =
    Maybe (Either NativeFile ImageSrc)


type alias Company =
    { id : CompanyId
    , name : CompanyName
    , logo : Logo
    , adress : Adress
    }


companyDefaultImg : String
companyDefaultImg =
    "http://localhost:1337/img/company-placeholder.png"


empty : Company
empty =
    { id = Nothing
    , name = ValidationInput.Ok ""
    , logo = Nothing
    , adress = Adress.Model.empty
    }


mtypeDecoder : Decode.Decoder (Maybe MimeType.MimeType)
mtypeDecoder =
    Decode.map MimeType.parseMimeType (Decode.field "type" Decode.string)


fileDecoder : Decode.Decoder NativeFile
fileDecoder =
    Decode.map4 NativeFile
        (Decode.field "name" Decode.string)
        (Decode.field "size" Decode.int)
        mtypeDecoder
        Decode.value


companyDecoder : Decode.Decoder Company
companyDecoder =
    decode (\id name -> Company id (pure name))
        |> required "id" (maybe string)
        |> required "name" string
        |> required "logo" (maybe (either fileDecoder string))
        |> required "adress" adressDecoder


companiesDecoder : Decode.Decoder (List Company)
companiesDecoder =
    Decode.list companyDecoder


companyEncoder : Company -> Encode.Value
companyEncoder company =
    let
        attributes =
            [ ( "name", Encode.string (ValidationInput.get company.name) )
            , ( "logo"
              , company.logo
                    |> Maybe.andThen leftToMaybe
                    |> Maybe.map .blob
                    |> Maybe.withDefault Encode.null
              )
            , ( "adress", adressEncoder company.adress )
            ]
    in
    Encode.object attributes
