module Position.Request exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import Position.Model exposing (Position, PositionId)
import RemoteData exposing (WebData)

type Msg
  = SavedPosition (WebData Position)

apiUrl : String
apiUrl = "http://localhost:4000/"

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
      [ ("description", Encode.string a.description)
      , ("unit", Encode.string a.unit)
      ]
  in
    Encode.object attributes

positionsUrl : String
positionsUrl =
  apiUrl ++ "positions/"

positionUrl : String -> String
positionUrl positionId =
  positionsUrl ++ positionId

savePosition : { a | description : String, unit : String } -> Cmd (WebData Position)
savePosition position =
  Http.post 
    (apiUrl ++ "positions")
    (positionEncoder position |> Http.jsonBody)
    positionDecoder
    |> RemoteData.sendRequest

fetchPositions : Cmd (WebData (List Position))
fetchPositions =
  Http.get 
    positionsUrl
    positionsDecoder
    |> RemoteData.sendRequest
    -- |> Cmd.map Msgs.OnFetchPositions

fetchPosition : PositionId -> Cmd (WebData Position)
fetchPosition positionId =
  Http.get
    (positionUrl positionId)
    positionDecoder
    |> RemoteData.sendRequest

deletePositionRequest : PositionId -> Http.Request PositionId
deletePositionRequest positionId =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = (positionUrl positionId)
    , body = Http.emptyBody
    , expect = Http.expectStringResponse (\_ -> Ok positionId)
    , timeout = Nothing
    , withCredentials = False
    }

deletePosition : PositionId -> Cmd (WebData PositionId)
deletePosition positionId =
  deletePositionRequest positionId
    |> RemoteData.sendRequest
  