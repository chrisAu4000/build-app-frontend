module User.Request exposing (LoginData, RegistrationData, loginUser, registerUser)

import Auth.Model exposing (Auth, authDecoder, authEncoder)
import Http
import Json.Encode as Encode
import RemoteData exposing (WebData)


apiUrl : String
apiUrl =
    "http://localhost:1337"


registrationUrl : String
registrationUrl =
    apiUrl ++ "/auth/local/register"


loginUrl : String
loginUrl =
    apiUrl ++ "/auth/local"


type alias LoginData =
    { email : String, password : String }


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


loginEncoder : LoginData -> Encode.Value
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


loginUser : LoginData -> Cmd (WebData Auth)
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
