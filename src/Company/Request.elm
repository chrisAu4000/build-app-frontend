module Company.Request exposing (createCompany, fetchCompanies, removeCompany, uploadImage)

import Http
import Company.Model exposing (Company, CompanyId, companyEncoder, companyDecoder, companiesDecoder)
import Auth.Model exposing (Token)
import RemoteData exposing (WebData)

import Json.Decode as Decode
import FileReader exposing (NativeFile)
import MimeType

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
    , body = companyEncoder company |> Http.jsonBody
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

uploadImageRequest : Token -> NativeFile -> Http.Request NativeFile
uploadImageRequest token file =
  Http.request
    { method = "POST"
    , headers = [ authorizationHeader token ]
    , url = "http://localhost:1337/upload"
    , body = Http.multipartBody
        [ Http.stringPart "part1" file.name
        , FileReader.filePart "upload" file
        ]
    , expect = Http.expectJson fileDecoder
    , timeout = Nothing
    , withCredentials = False
    }

uploadImage : Token -> NativeFile -> Cmd (WebData NativeFile)
uploadImage token file =
  uploadImageRequest token file
    |> RemoteData.sendRequest