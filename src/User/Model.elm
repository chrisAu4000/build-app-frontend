module User.Model exposing
  ( User
  , UserId
  , Username
  , Email
  , Password
  , userDecoder
  , userEncoder
  )

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode

type alias UserId = String
type alias Username = String
type alias Password = String
type alias Email = String
-- type alias Token = String

-- type alias Auth =
--   { jwt : Token
--   , user : User
--   }

type alias User =
  { id : UserId
  , email : Email
  , provider : String
  , username : Username
  }

userDecoder : Decode.Decoder User 
userDecoder =
  decode User
    |> required "_id" Decode.string
    |> required "email" Decode.string
    |> required "provider" Decode.string
    |> required "username" Decode.string

userEncoder : User -> Encode.Value
userEncoder user =
  Encode.object
    [ ("_id", Encode.string user.id)
    , ("provider", Encode.string user.provider)
    , ("username", Encode.string user.username)
    , ("email", Encode.string user.email)
    ]