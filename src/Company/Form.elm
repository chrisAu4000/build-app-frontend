module Company.Form exposing (..)

import Json.Decode as Decode
import Company.Validation exposing (validateCompanyName)
import Company.Request exposing (uploadImage)
import Component.Form exposing (labeledTextInput, labeledFileInput)
import Html exposing (Attribute, Html, div, form, input, img, text)
import Html.Attributes exposing (class, id, value, type_, accept, src, type_)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode as Decode
import FileReader exposing (NativeFile)
import Task
import Validation exposing (Validation, (<*>), toBool)
import RemoteData exposing (WebData)

type alias Name = String

type alias Model =
  { name : Name
  , nameError : Validation (List String) Name
  , logo : Maybe NativeFile
  , imgUrl : Maybe String
  , isValid : Bool
  }

type Msg
  = InputCompanyName Name
  | InputCompanyLogo (List NativeFile)
  | OnDataUrl (Result FileReader.Error FileReader.FileContentDataUrl)
  | Upload
  | OnUpload (WebData NativeFile)

init : Model
init =
  { name = ""
  , nameError = Validation.Err []
  , logo = Nothing
  , imgUrl = Nothing
  , isValid = False
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputCompanyName name ->
      let
        newModel = { model | name = name, nameError = validateCompanyName name }
      in
      ({ newModel | isValid = validateCompanyForm newModel |> toBool }, Cmd.none )
    InputCompanyLogo files ->
      case files of
        [ file ] ->
          ( { model | logo = Just file }
          , FileReader.readAsDataUrl file.blob
            |> Task.attempt OnDataUrl
          )
        _ -> (model, Cmd.none)
    OnDataUrl url ->
      case Result.mapError FileReader.prettyPrint url
        |> Result.andThen (Decode.decodeValue Decode.string) of
        Err err -> (model, Cmd.none)
        Ok urlS -> ({ model | imgUrl = Just urlS }, Cmd.none)
    Upload ->
        ( model
        ,  model.logo
          |> Maybe.map (uploadImage "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJfaWQiOiI1YWExYmQxMDBjZjFmMDE5ZDJmZTYyNzUiLCJpZCI6bnVsbCwiaWF0IjoxNTIwNjc1ODQ2LCJleHAiOjE1MjMyNjc4NDZ9.CgrmDf05_9fYpUUJBcTLVOQUlvc0VP_PlyQIqWKU1v8")
          |> Maybe.map (Cmd.map OnUpload)
          |> Maybe.withDefault Cmd.none
        )
       
    OnUpload re ->
      let
        _ = Debug.log "re" (toString re)
      in
        ( model, Cmd.none )

renderImg : Maybe String -> Html Msg
renderImg url =
  case url of
    Nothing -> img [ class "company-logo__empty" ] []
    Just url -> 
      div
        [ class "flex-align-middle"]
        [ img 
          [ class "company-logo"
          , src url 
          ]
          []
        ]

view : Model -> Html Msg
view model =
  form
    [ class "company-name-form"
    , onSubmit Upload
    ]
    [ div
      [ class "col col-12" ]
      [ div
        [ class "image-input-wrapper mb2 right col-6 flex flex-column"]
        [ renderImg model.imgUrl
        , labeledFileInput "company-logo" "Company Logo"
          [ class "image-input p0 m0 border-none"
          , accept ".jpg, .jpeg, .png"
          , FileReader.onFileChange InputCompanyLogo
          ]
          []
        ]
      ]
    , div
      [ class "col col-12"]
      [ labeledTextInput "Company Name"
        [ value model.name
        , onInput InputCompanyName
        ]
        []
      ]
    , input
        [ type_ "submit" ]
        [ text "upload "]
    ]

validateCompanyForm : Model -> Validation (List String) Name
validateCompanyForm model = model.nameError
