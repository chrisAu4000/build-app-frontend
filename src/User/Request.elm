module User.Request exposing (RegistrationData, loginUser, registerUser, authDecoder, authEncoder)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import RemoteData exposing (WebData)
import User.Model exposing (User, AuthResponse, userDecoder, userEncoder)

apiUrl : String
apiUrl = "http://localhost:1337"
authUrl : String
authUrl = "/auth/local/register"
loginUrl : String
loginUrl = "/auth/local"

type alias UserLike a =
  { a | email : String, password : String }

type alias Auth a =
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

loginEncoder : UserLike a -> Encode.Value
loginEncoder userlike =
  let
    attributes =
      [ ( "identifier", Encode.string userlike.email )
      , ( "password", Encode.string userlike.password )
      ]
  in
    Encode.object attributes

userRegistrationEncoder : Auth a -> Encode.Value
userRegistrationEncoder data =
  let
    attributes =
      [ ( "username", Encode.string data.username )
      , ( "email", Encode.string data.email )
      , ( "password", Encode.string data.password )
      ]
  in
    Encode.object attributes

authDecoder : Decode.Decoder AuthResponse
authDecoder =
  decode AuthResponse
    |> required "jwt" Decode.string
    |> required "user" userDecoder

authEncoder : AuthResponse -> Encode.Value
authEncoder auth =
  let
    attributes =
      [ ("jwt", Encode.string auth.jwt)
      , ("user", userEncoder auth.user)
      ]
  in
    Encode.object attributes

loginUser : UserLike a -> Cmd (WebData AuthResponse)
loginUser user =
  Http.post
    (apiUrl ++ loginUrl)
    (loginEncoder user |> Http.jsonBody)
    authDecoder
    |> RemoteData.sendRequest

registerUser : RegistrationData -> Cmd (WebData AuthResponse)
registerUser user = 
  Http.post
    (apiUrl ++ authUrl)
    (userRegistrationEncoder user |> Http.jsonBody)
    authDecoder
      |> RemoteData.sendRequest