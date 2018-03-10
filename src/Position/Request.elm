module Position.Request exposing (..)

import Http
import Position.Model exposing
  ( Position
  , PositionId
  , positionDecoder
  , positionsDecoder
  , positionEncoder
  )
import RemoteData exposing (WebData)

type Msg
  = SavedPosition (WebData Position)

apiUrl : String
apiUrl = "http://localhost:4000/"

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
  