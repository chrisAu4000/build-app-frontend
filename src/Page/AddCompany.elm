module Page.AddCompany exposing (..)

-- import Company.Wizard as Wizard

import Auth.Model exposing (Auth)
import Company.Form as CompanyForm
import Company.Model exposing (CompanyId)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, style)


type alias Model =
    { auth : Auth
    , company : CompanyForm.Model
    }


type Msg
    = CompanyMsg CompanyForm.Msg


init : Auth -> CompanyId -> ( Model, Cmd Msg )
init auth companyId =
    let
        ( model, cmd ) =
            CompanyForm.init auth.jwt companyId
    in
    ( { auth = auth, company = model }
    , cmd |> Cmd.map CompanyMsg
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompanyMsg msg ->
            let
                ( company, companyMsg ) =
                    CompanyForm.update msg model.company
            in
            ( { model | company = company }, Cmd.map CompanyMsg companyMsg )


view : Model -> Html Msg
view model =
    div
        [ class "page m2 flex flex-column" ]
        [ h1 [ class "mx-auto col sm-col-12 md-col-9 lg-col-6" ] [ text "Add a Company" ]
        , CompanyForm.view model.company |> Html.map CompanyMsg
        ]
