module Page.Home exposing (Model, Msg, init, update, view)

import Component.Button exposing (successBtn)
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class)
import User.Model exposing (AuthResponse)

type alias Model =
  { auth : AuthResponse }

type Msg
  = Msg

init : AuthResponse -> (Model, Cmd Msg)
init auth = ( { auth = auth }, Cmd.none )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Msg -> (model, Cmd.none)

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

view : Model -> Html Msg
view model =
  div
    [ class "clearfix page-home flex flex-column"]
    [ h1 [class "ml2"] [(text ("Hi " ++ model.auth.user.username))]
    , jumbotron
    -- , if (List.length model.user.companies) == 0
    --   then jumbotron
    --   else div [] [ text "company-list" ]
    ]