module Page.Home exposing (Model, Msg, init, update, view)

import Auth.Model exposing (Auth)
import Company.Grid as CompanyGrid
import Component.Button exposing (successBtn)
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class)
import RemoteData exposing (WebData)


type alias Model =
    { auth : Auth
    , companyGrid : CompanyGrid.Model
    , listEmpty : Bool
    }


type Msg
    = CompanyGridMsg CompanyGrid.Msg


init : Auth -> ( Model, Cmd Msg )
init auth =
    let
        ( subMod, subCmd ) =
            CompanyGrid.init auth.jwt
    in
    ( { auth = auth, companyGrid = subMod, listEmpty = False }
    , subCmd |> Cmd.map CompanyGridMsg
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompanyGridMsg subMsg ->
            let
                ( newModel, cmd ) =
                    CompanyGrid.update subMsg model.companyGrid

                listEmpty =
                    companyGridEmpty newModel.companies
            in
            ( { model | companyGrid = newModel, listEmpty = listEmpty }
            , cmd |> Cmd.map CompanyGridMsg
            )


view : Model -> Html Msg
view model =
    let
        companyList =
            CompanyGrid.view model.companyGrid |> Html.map CompanyGridMsg
    in
    div
        [ class "clearfix page m2 flex flex-column mx-auto s-col-12 md-col-11 lg-col-9" ]
        [ h1 [ class "ml2" ] [ text ("Hi " ++ model.auth.user.username) ]
        , if model.listEmpty then
            jumbotron
          else
            companyList
        ]


jumbotronStr : String
jumbotronStr =
    "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."


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


companyGridEmpty : WebData (List a) -> Bool
companyGridEmpty webData =
    case webData of
        RemoteData.Success list ->
            List.length list == 0

        RemoteData.Failure err ->
            False

        _ ->
            False
