module Page.AddPosition exposing (Model, Msg, init, update, view)

import Component.Sidebar exposing (sidebar)
import Html exposing (Html, div, h1, input, text)
import Position.List as PositionList
import Position.Form as PositionForm

type alias Model =
  { positionForm : PositionForm.Model
  , positionList : PositionList.Model
  }

type Msg
  = FormMsg PositionForm.Msg
  | ListMsg PositionList.Msg

init : (Model, Cmd Msg)
init =
  let
    (formModel, formCmd) = PositionForm.initNew
    (listModel, listCmd) = PositionList.initNew
  in
    ( Model formModel listModel
    , Cmd.batch
      [ formCmd |> Cmd.map FormMsg
      , listCmd |> Cmd.map ListMsg
      ]
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FormMsg subMsg ->
      let
        (subModel, subCmd) =
          (PositionForm.update subMsg model.positionForm)
        _ = Debug.log "msg" (toString subCmd)
      in
        ( { model | positionForm = subModel }, Cmd.map FormMsg subCmd)
    ListMsg subMsg ->
      let 
        (subModel, subCmd) =
          PositionList.update subMsg model.positionList
      in
        ( { model | positionList = subModel }, Cmd.map ListMsg subCmd)

view : Model -> Html Msg
view model =
  let
    positionForm = PositionForm.view model.positionForm |> Html.map FormMsg
    positionList = PositionList.view model.positionList |> Html.map ListMsg
  in
    div
      [] 
      [ positionList
      , positionForm
      ]
