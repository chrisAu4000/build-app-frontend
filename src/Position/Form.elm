module Position.Form exposing (Model, Msg, initNew, init, update, view)

import Component.Input exposing (labeledTextInput, labeledTextArea)
import Component.Button exposing (submitBtnNotAsked, submitBtnLoading, submitBtnFailure, submitBtnSuccess)
import Html exposing (Attribute, Html, div, form, text)
import Html.Attributes exposing (class, value, required)
import Html.Events exposing (onInput, onSubmit)
import Position.Model exposing (Position, PositionId)
import Position.Request exposing (fetchPosition, savePosition)
import RemoteData exposing (WebData)

type alias Model =
  { position : WebData Position
  , unit : String
  , description : String
  , btnState : WebButton String
  }

type Msg
  = UnitInput String
  | DescriptionInput String
  | Submit String String
  | OnReceivedPosition (WebData Position)
  | OnSavedPosition (WebData Position)
  | OnSavedPositionFailed String

type WebButton a
  = NotAsked a
  | Loading a
  | Failure a
  | Success a

btnStateNotAsked : WebButton String
btnStateNotAsked = NotAsked "Save Position"

btnStateLoading : WebButton String
btnStateLoading = Loading "Saving Position ..."

btnStateFailure : WebButton String
btnStateFailure = Failure "Sorry... Can't save Position"

btnStateSuccess : WebButton String
btnStateSuccess = Success "Yey Saved Position"

initNew : (Model, Cmd msg)
initNew =
  ( Model RemoteData.NotAsked  "" "" btnStateNotAsked
  , Cmd.none
  )

init : PositionId -> (Model, Cmd Msg)
init positionId =
  ( Model RemoteData.Loading "" "" btnStateLoading
  , fetchPosition positionId 
      |> Cmd.map OnReceivedPosition
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UnitInput unit ->
      ( { model | unit = unit, btnState = btnStateNotAsked }, Cmd.none )
    DescriptionInput description ->
      ( { model | description = description, btnState = btnStateNotAsked }, Cmd.none )
    Submit unit description ->
      ( { model | btnState = btnStateLoading }
      , savePosition { unit = unit, description = description }
        |> Cmd.map OnSavedPosition
      )
    OnReceivedPosition position ->
      ( { model | position = position }
      , Cmd.none
      )
    OnSavedPosition position ->
      case position of
        RemoteData.NotAsked -> 
          ( { model | btnState = btnStateNotAsked }, Cmd.none )
        RemoteData.Loading ->
          ( { model | btnState = btnStateLoading }, Cmd.none )
        RemoteData.Failure err ->
          ( { model | btnState = btnStateFailure }, Cmd.none )
        RemoteData.Success pos ->
          ( { model |
              btnState = btnStateSuccess
            , unit = ""
            , description = ""
            }
            , Cmd.none )
    OnSavedPositionFailed err -> 
      ( { model | btnState = btnStateFailure }, Cmd.none)
    
positionForm : List (Attribute msg) -> List (Html msg) -> Html msg
positionForm attrs children =
  let
    attributes = attrs ++ [ class "clearfix m1 position form" ]
  in
    div 
      [ class "clearfix align-h-middle col col-6" ]
      [ form attributes children ]

textInputUnit : String -> Html Msg
textInputUnit val =
  labeledTextInput "Unit" 
    [ value val
    , onInput UnitInput
    ] 

textAreaDescription : String -> Html Msg
textAreaDescription val =
  labeledTextArea "Description"
    [ value val
    , onInput DescriptionInput 
    ] 
    []

submitBtn : WebButton String -> Html msg
submitBtn state =
  case state of
    NotAsked l -> submitBtnNotAsked l
    Loading l -> submitBtnLoading l
    Failure l -> submitBtnFailure l
    Success l -> submitBtnSuccess l

view : Model -> Html Msg
view model =
  positionForm 
    [ onSubmit (Submit model.unit model.description) ]
    [ textInputUnit model.unit
    , textAreaDescription model.description
    , submitBtn model.btnState
    , text (toString model)
    ]