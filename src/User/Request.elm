module User.Request exposing (RegistrationData, loginUser, registerUser)

import Http
import Json.Encode as Encode
import RemoteData exposing (WebData)
import Auth.Model exposing (Auth, authDecoder, authEncoder)

apiUrl : String
apiUrl = "http://localhost:1337"
registrationUrl : String
registrationUrl = apiUrl ++ "/auth/local/register"
loginUrl : String
loginUrl = apiUrl ++ "/auth/local"

type alias Login a =
  { a | email : String, password : String }

type alias Registration a =
  { a
  | username : String
  , email : String
  , password : String
  }

type alias RegistrationData =
  { username : String
  , email : String
  , password : String
  , passwordVerification : String
  }

loginEncoder : Login a -> Encode.Value
loginEncoder userlike =
  let
    attributes =
      [ ( "identifier", Encode.string userlike.email )
      , ( "password", Encode.string userlike.password )
      ]
  in
    Encode.object attributes

userRegistrationEncoder : Registration a -> Encode.Value
userRegistrationEncoder data =
  let
    attributes =
      [ ( "username", Encode.string data.username )
      , ( "email", Encode.string data.email )
      , ( "password", Encode.string data.password )
      ]
  in
    Encode.object attributes

loginUser : Login a -> Cmd (WebData Auth)
loginUser user =
  Http.post
    loginUrl
    (loginEncoder user |> Http.jsonBody)
    authDecoder
      |> RemoteData.sendRequest

registerUser : RegistrationData -> Cmd (WebData Auth)
registerUser user = 
  Http.post
    registrationUrl
    (userRegistrationEncoder user |> Http.jsonBody)
    authDecoder
      |> RemoteData.sendRequest