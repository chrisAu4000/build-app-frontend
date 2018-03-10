module Company.Model exposing (..)

import Adress.Model exposing (Adress, Street, HouseNr, PostCode, Domicile, adressDecoder, adressEncoder)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import Validation exposing (Validation, (<*>), minLength, maxLength)
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
  , adress : Adress
  }

type alias ValidCompany = Validation (List String) Company

type alias ValidCompanyName = Validation (List String) CompanyName

empty : Company
empty =
  { id = Nothing
  , name = ""
  , adress = Adress.Model.empty
  }

companyDecoder : Decode.Decoder Company
companyDecoder =
  decode Company
    |> required "id" (Decode.maybe Decode.string)
    |> required "name" Decode.string
    |> required "adress" adressDecoder

companiesDecoder : Decode.Decoder (List Company)
companiesDecoder = Decode.list companyDecoder

companyEncoder : Company -> Encode.Value
companyEncoder company =
  let
    attributes =
      [ ( "name", Encode.string company.name )
      , ( "adress", adressEncoder company.adress )
      ]
  in
    Encode.object attributes
