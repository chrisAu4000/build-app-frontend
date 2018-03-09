module Company.List exposing (..)

import Company.Model exposing (Company)
import Company.Request exposing (fetchCompanies)
import Component.Loader exposing (load)
import Html exposing (Html, li, ul, text)
import Html.Attributes exposing (class)
import RemoteData exposing (WebData)
import User.Model exposing (Token)

type alias Model = WebData (List Company)

type Msg
  = OnReceivedCompanies Model

init : Token -> (Model, Cmd Msg)
init token =
  ( RemoteData.Loading 
  , fetchCompanies token
    |> Cmd.map OnReceivedCompanies
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnReceivedCompanies companies -> ( companies, Cmd.none)

listItem : Company -> Html Msg
listItem company =
  li
    [ class "company-name-listitem" ]
    [ text company.name ]

list : Model -> List (Html Msg)
list companies =
  case companies of
    RemoteData.Loading -> [ load ]
    RemoteData.Failure err -> [ text (toString err) ]
    RemoteData.Success cms -> List.map listItem cms
    RemoteData.NotAsked -> [ text "" ]

view : Model -> Html Msg
view model =
  ul 
    [ class "company-name-list" ]
    ( list model )
