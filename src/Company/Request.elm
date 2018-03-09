module Company.Request exposing (createCompany, fetchCompanies)

import Adress.Request exposing (adressEncoder, adressDecoder)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import Company.Model exposing (Company)
import User.Model exposing (Token)
import RemoteData exposing (WebData)

apiUrl : String
apiUrl = "http://localhost:1337/company"

companyDecoder : Decode.Decoder Company
companyDecoder =
  decode Company
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

authorizationHeader : Token -> Http.Header
authorizationHeader token = Http.header "Authorization" ("Bearer " ++ token)

listCompaniesRequest : Token -> Http.Request (List Company)
listCompaniesRequest token =
  Http.request
    { method = "GET"
    , headers = [ authorizationHeader token ]
    , url = apiUrl
    , body = Http.emptyBody
    , expect = Http.expectJson companiesDecoder
    , timeout = Nothing
    , withCredentials = False
    }
createCompanyRequest : Token -> Company -> Http.Request Company
createCompanyRequest token company =
  Http.request
    { method = "POST"
    , headers = [ authorizationHeader token ]
    , url = apiUrl
    , body = companyEncoder company |> Http.jsonBody
    , expect = Http.expectJson companyDecoder
    , timeout = Nothing
    , withCredentials = False
    }

createCompany : Token -> Company -> Cmd (WebData Company)
createCompany token company =
  createCompanyRequest token company
    |> RemoteData.sendRequest

fetchCompanies : Token -> Cmd (WebData (List Company))
fetchCompanies token =
  listCompaniesRequest token
    |> RemoteData.sendRequest