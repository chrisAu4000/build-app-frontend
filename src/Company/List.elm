module Company.List exposing (..)

import Company.Model exposing (Company, CompanyId)
import Company.Request exposing (fetchCompanies, removeCompany)
import Component.Button exposing (editBtn, deleteBtn, listHeaderBtn)
import Component.Icon as Icon
import Component.Loader exposing (load)
import Html exposing (Html, div, li, ul, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import RemoteData exposing (WebData)
import User.Model exposing (AuthResponse, Token)

type alias Companies = WebData (List Company)

type alias Model =
  { token : Token
  , companies : Companies
  }

type Msg
  = OnFetch Companies
  | Remove CompanyId
  | OnRemove (WebData Company)

init : Token -> (Model, Cmd Msg)
init token =
  ( { token = token, companies = RemoteData.Loading }
  , fetchCompanies token |> Cmd.map OnFetch
  )

remove : CompanyId -> Companies -> Companies
remove id =
  RemoteData.map (List.filter (not << ((==) id) << .id))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnFetch companies ->
      ({ model | companies = companies }, Cmd.none)
    Remove maybeId ->
      case maybeId of
        Nothing ->
          let
            _ = Debug.log "ERROR" "ID is nothing"
          in
            (model, Cmd.none)
        Just id ->
          (model, (removeCompany model.token id) |> Cmd.map OnRemove)
    OnRemove company ->
      case company of
        RemoteData.Loading -> (model, Cmd.none)
        RemoteData.Failure error -> 
          let
            _ = Debug.log "ERROR" (toString error)
          in
            (model, Cmd.none)
        RemoteData.Success company ->
          let
            _ = Debug.log "SUCCES" (toString company)
          in
            ( {model | companies = remove company.id model.companies}, Cmd.none)
        RemoteData.NotAsked -> (model, Cmd.none)

buttonItem : Int -> Int -> Html msg -> Html msg
buttonItem x y child =
  div 
    [ class ("flex px2 align-v-middle col col-" ++ (toString x)) 
    , style [ ( "word-wrap", "break-word" )]
    ] 
    [ child ]

listItem : Company -> Html Msg
listItem company =
  li
    [ class "company-name-listitem list-item clearfix li" ]
    [ div 
      [ class "flex full-height"] 
      [ buttonItem 6 2 (text company.name)
      , buttonItem 3 2 (editBtn "" )
      , buttonItem 3 2 (deleteBtn [ onClick (Remove company.id) ])
      ]
    ]

list : Companies -> List (Html Msg)
list companies =
  case companies of
    RemoteData.Loading -> [ load ]
    RemoteData.Failure err -> [ text (toString err) ]
    RemoteData.Success cms -> List.map listItem cms
    RemoteData.NotAsked -> [ text "" ]

view : Model -> Html Msg
view model =
  div
    [ class "clearfix m2" ]
    [ div
      [ class "clearfix button-bar" ]
      [ listHeaderBtn (Icon.Plus, "#/company") ]
    , ul 
      [ class "list-reset company-name-list" ]
      ( list model.companies )
    ]
  