module Page.Home exposing (Model, Msg, init, update, view)

import Company.List as CompanyList
import Component.Button exposing (successBtn)
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class)
import User.Model exposing (Auth)
import RemoteData exposing (WebData)

type alias Model =
  { auth : Auth
  , companyList : CompanyList.Model
  , listEmpty : Bool
  }

type Msg
  = CompanyListMsg CompanyList.Msg

init : Auth -> (Model, Cmd Msg)
init auth =
  let
    (subMod, subCmd) = CompanyList.init auth.jwt
  in
    ( { auth = auth, companyList = subMod, listEmpty = False }
    , subCmd |> Cmd.map CompanyListMsg
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CompanyListMsg subMsg ->
      let
        (newModel, cmd) =
          CompanyList.update subMsg model.companyList
        listEmpty = companyListEmpty newModel.companies
      in 
        ( { model | companyList = newModel, listEmpty = listEmpty }
        , cmd |> Cmd.map CompanyListMsg
        )

view : Model -> Html Msg
view model =
  let
    companyList =
      CompanyList.view model.companyList |> Html.map CompanyListMsg
  in
    div
      [ class "clearfix page-home flex flex-column"]
      [ h1 [class "ml2"] [(text ("Hi " ++ model.auth.user.username))]
      , if model.listEmpty
        then jumbotron
        else companyList
      ]

jumbotronStr : String
jumbotronStr = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."

jumbotron : Html Msg
jumbotron =
  div
    [ class "jumbotron rounded align-h-middle bg-blue col-9" ]
    [ h1
      [ class "jumbotron-h1 h1 center white" ]
      [ text "BuildApp" ]
    , p
      [ class "jumbotron-p white m1 p1" ]
      [ text jumbotronStr ]
    , div
      [ class "flex m2" ]
      [ successBtn "#/company" "Start" ]
    ]

companyListEmpty : WebData (List a) -> Bool
companyListEmpty webData =
  case webData of
    RemoteData.Success list -> List.length list == 0
    RemoteData.Failure err -> False
    _ -> False