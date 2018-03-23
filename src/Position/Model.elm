module Position.Model
    exposing
        ( FormModel(..)
        , FormState
        , Position
        , PositionId
        , positionDecoder
        , positionEncoder
        , positionsDecoder
        )

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode


type alias PositionId =
    String


type alias Errors =
    List String


type alias FormState =
    { errors : Errors
    , description : String
    , unit : String
    }


type FormModel
    = NotAsked FormState
    | Loading FormState
    | Success FormState
    | Failure FormState


type alias Position =
    { id : PositionId
    , description : String
    , unit : String
    }


positionDecoder : Decode.Decoder Position
positionDecoder =
    decode Position
        |> required "id" Decode.string
        |> required "description" Decode.string
        |> required "unit" Decode.string


positionsDecoder : Decode.Decoder (List Position)
positionsDecoder =
    Decode.list positionDecoder


positionEncoder : { a | description : String, unit : String } -> Encode.Value
positionEncoder a =
    let
        attributes =
            [ ( "description", Encode.string a.description )
            , ( "unit", Encode.string a.unit )
            ]
    in
    Encode.object attributes
