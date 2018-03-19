module Company.Model exposing (..)

import Adress.Model exposing (Adress, Street, HouseNr, PostCode, Domicile, adressDecoder, adressEncoder)
import Either exposing (Either)
import Either.Decode as DecodeE
import FileReader exposing (NativeFile)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import Validation exposing (Validation, (<*>), minLength, maxLength)
import MimeType

type alias CompanyName = String
type alias CompanyId = Maybe String
type alias ErrorMsg = String

-- type alias BankAccount =
--   { bankName : String
--   , 
--   }
type alias Company =
  { id : CompanyId
  , name : CompanyName
  , logo : Maybe (Either NativeFile String)
  , adress : Adress
  }

type alias ValidCompany = Validation (List String) Company

type alias ValidCompanyName = Validation (List String) CompanyName

empty : Company
empty =
  { id = Nothing
  , name = ""
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
  decode Company
    |> required "id" (Decode.maybe Decode.string)
    |> required "name" Decode.string
    |> required "logo" (Decode.maybe (DecodeE.either fileDecoder Decode.string))
    |> required "adress" adressDecoder

companiesDecoder : Decode.Decoder (List Company)
companiesDecoder = Decode.list companyDecoder

companyEncoder : Company -> Encode.Value
companyEncoder company =
  let
    attributes =
      [ ( "name", Encode.string company.name )
      , ( "logo"
        , company.logo
          |> Maybe.andThen (Either.leftToMaybe)
          |> Maybe.map .blob 
          |> Maybe.withDefault Encode.null
        )
      , ( "adress", adressEncoder company.adress )
      ]
  in
    Encode.object attributes
