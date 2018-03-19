module Company.Request exposing (createCompany, fetchCompanies, removeCompany)

import Auth.Model exposing (Token)
import Company.Model exposing (Company, CompanyId, companyEncoder, companyDecoder, companiesDecoder)
import Either exposing (Either)
import FileReader exposing (NativeFile)
import Http
import Json.Decode as Decode
import MimeType
import RemoteData exposing (WebData)

apiUrl : String
apiUrl = "http://localhost:1337/company"

authorizationHeader : Token -> Http.Header
authorizationHeader token = Http.header "Authorization" ("Bearer " ++ token)

createCompanyRequest : Token -> Company -> Http.Request Company
createCompanyRequest token company =
  Http.request
    { method = "POST"
    , headers = [ authorizationHeader token ]
    , url = apiUrl
    -- , body = companyEncoder company |> Http.jsonBody
    , body = Http.multipartBody
      [ Http.stringPart "name" company.name
      , (Maybe.withDefault
          (Http.stringPart "files" "null")
          (company.logo
            |> Maybe.andThen Either.leftToMaybe
            |> Maybe.map (FileReader.filePart "files")
          )
        )
      , Http.stringPart "street" company.adress.street
      , Http.stringPart "postCode" (company.adress.postCode)
      , Http.stringPart "houseNr" company.adress.houseNr
      , Http.stringPart "domicile" company.adress.domicile
      ]
    , expect = Http.expectJson companyDecoder
    , timeout = Nothing
    , withCredentials = False
    }

fetchCompaniesRequest : Token -> Http.Request (List Company)
fetchCompaniesRequest token =
  Http.request
    { method = "GET"
    , headers = [ authorizationHeader token ]
    , url = apiUrl
    , body = Http.emptyBody
    , expect = Http.expectJson companiesDecoder
    , timeout = Nothing
    , withCredentials = False
    }

removeCompanyRequest : Token -> String -> Http.Request (Company)
removeCompanyRequest token id =
  Http.request
    { method = "DELETE"
    , headers = [ authorizationHeader token ]
    , url = apiUrl ++ "/" ++ id
    , body = Http.emptyBody
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
  fetchCompaniesRequest token
    |> RemoteData.sendRequest

removeCompany : Token -> String -> Cmd (WebData Company)
removeCompany token id =
  removeCompanyRequest token id
    |> RemoteData.sendRequest

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
