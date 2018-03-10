module Auth.Model exposing (..)

import Json.Decode as Decode exposing (Value)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import Ports
import User.Model exposing (User, userDecoder, userEncoder)

type alias Token = String

type alias Auth =
  { jwt : Token
  , user : User
  }

authDecoder : Decode.Decoder Auth
authDecoder =
  decode Auth
    |> required "jwt" Decode.string
    |> required "user" userDecoder

authEncoder : Auth -> Encode.Value
authEncoder auth =
  let
    attributes =
      [ ("jwt", Encode.string auth.jwt)
      , ("user", userEncoder auth.user)
      ]
  in
    Encode.object attributes

decodeAuth : Value -> Maybe Auth
decodeAuth val =
  Decode.decodeValue Decode.string val
    |> Result.toMaybe
    |> Maybe.andThen (Decode.decodeString authDecoder >> Result.toMaybe)

storeAuth : Auth -> Cmd msg
storeAuth auth =
    authEncoder auth
      |> Encode.encode 0
      |> Just
      |> Ports.storeSession

authChange : Sub (Maybe Auth)
authChange =
  Ports.onSessionChange (Decode.decodeValue authDecoder >> Result.toMaybe)
