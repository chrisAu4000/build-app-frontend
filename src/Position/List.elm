module Position.List exposing (..)

import Component.Button exposing (deleteBtn, editBtn)
import Component.Loader exposing (load)
import Html exposing (..)
import Html.Attributes exposing (class, draggable, href, style)
import Html.Events exposing (onClick)
import Position.Model exposing (Position, PositionId)
import Position.Request as Requests
    exposing
        ( deletePosition
        , fetchPositions
        , positionUrl
        )
import RemoteData exposing (WebData)


type alias Model =
    WebData (List Position)


type Msg
    = OnReceivedPositions Model
    | RemovePosition PositionId
    | OnRemovedPosition (WebData PositionId)
    | RequestsMsgs Requests.Msg


initNew : ( Model, Cmd Msg )
initNew =
    ( RemoteData.Loading
    , fetchPositions
        |> Cmd.map OnReceivedPositions
    )


removePosition : PositionId -> Model -> Model
removePosition idToRemove =
    let
        filterPositionList =
            List.filter (\position -> position.id /= idToRemove)
    in
    RemoteData.map (\list -> filterPositionList list)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnReceivedPositions positions ->
            ( positions, Cmd.none )

        RemovePosition positionId ->
            ( model, deletePosition positionId |> Cmd.map OnRemovedPosition )

        OnRemovedPosition webPosition ->
            case webPosition of
                RemoteData.Failure err ->
                    let
                        _ =
                            Debug.log "error" (toString err)
                    in
                    ( model, Cmd.none )

                RemoteData.Success id ->
                    ( removePosition id model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RequestsMsgs msg ->
            case msg of
                Requests.SavedPosition webPosition ->
                    case webPosition of
                        RemoteData.Failure error ->
                            ( model, Cmd.none )

                        RemoteData.Success position ->
                            ( RemoteData.map (\posList -> [ position ] ++ posList) model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )


positionItem : Int -> Int -> Html msg -> Html msg
positionItem x y child =
    div
        [ class ("flex px2 py" ++ toString y ++ " col col-" ++ toString x)
        , style [ ( "word-wrap", "break-word" ) ]
        ]
        [ child ]


listItem : Position -> Html Msg
listItem pos =
    li
        [ class "position list-item clearfix li mx1 mb1"
        , draggable "true"
        ]
        [ positionItem 6 1 (text pos.description)
        , positionItem 2 1 (text pos.unit)
        , positionItem 4 0 (deleteBtn [ onClick (RemovePosition pos.id) ])
        ]


selectedListItem : Position -> Html msg
selectedListItem pos =
    li
        [ class "position list-item clearfix li mx1 mb1 active"
        , draggable "true"
        ]
        [ positionItem 12 1 (text pos.description)
        ]


unselectedListItem : Position -> Html msg
unselectedListItem pos =
    li
        [ class "position list-item clearfix li mx1 mb1 unselected" ]
        [ positionItem 12 1 (text pos.description) ]


renderListItem : Maybe PositionId -> Position -> Html Msg
renderListItem positionId position =
    case positionId of
        Just id ->
            if position.id == id then
                selectedListItem position
            else
                unselectedListItem position

        Nothing ->
            listItem position


list : List Position -> Html Msg
list positions =
    div [ class "clearfix positions-list sidebar-item" ]
        [ ul
            [ class "list-reset " ]
            (List.map listItem positions)
        ]


maybeList : WebData (List Position) -> Html Msg
maybeList response =
    case response of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            load

        RemoteData.Success positions ->
            list positions

        RemoteData.Failure error ->
            text (toString error)


view : Model -> Html Msg
view model =
    div [ class "position-list clearfix" ]
        [ div [ class "sidebar-body pt1" ]
            [ maybeList model ]
        ]
