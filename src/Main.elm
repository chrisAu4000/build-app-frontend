module Main exposing (..)

import Auth.Model exposing (Auth, authChange, authDecoder, authEncoder, decodeAuth, storeAuth)
import Component.Navigation exposing (privateNavigation, publicNavigation)
import Html exposing (Html, div, h1, h3, text)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Page.AddCompany as AddCompanyPage
import Page.AddPosition as AddPositionPage
import Page.Company as CompanyPage
import Page.Home as HomePage
import Page.Login as LoginPage
import Page.NotFound as NotFoundPage
import Page.Registration as RegistrationPage
import Routing
import UrlParser exposing (Parser, oneOf, parseHash, s, top)


type Page
    = NotFound NotFoundPage.Model
    | Login LoginPage.Model
    | Registration RegistrationPage.Model
    | Home HomePage.Model
    | AddCompany AddCompanyPage.Model
    | Company CompanyPage.Model
    | AddPosition AddPositionPage.Model


type alias Model =
    { page : Page
    , auth : Maybe Auth
    }


initialModel : Maybe Auth -> Model
initialModel auth =
    { page = NotFound NotFoundPage.initialModel
    , auth = auth
    }


type Msg
    = LocationChanged Location
    | SetAuth (Maybe Auth)
    | LoginMsg LoginPage.Msg
    | RegistrationMsg RegistrationPage.Msg
    | HomeMsg HomePage.Msg
    | AddCompanyMsg AddCompanyPage.Msg
    | CompanyMsg CompanyPage.Msg
    | AddPositionMsg AddPositionPage.Msg


init : Value -> Location -> ( Model, Cmd Msg )
init authVal location =
    let
        auth =
            decodeAuth authVal

        ( newPage, msg ) =
            pageFromLocation (initialModel auth) location
    in
    ( { page = newPage, auth = auth }, msg )


pageFromLocation : Model -> Location -> ( Page, Cmd Msg )
pageFromLocation model location =
    case parseHash Routing.matchers location of
        Just route ->
            case route of
                Routing.NotFound ->
                    ( NotFound NotFoundPage.initialModel, Cmd.none )

                Routing.Login ->
                    let
                        ( model, msg ) =
                            LoginPage.init
                    in
                    ( Login model, Cmd.map LoginMsg msg )

                Routing.Registration ->
                    let
                        ( model, msg ) =
                            RegistrationPage.init
                    in
                    ( Registration model, Cmd.map RegistrationMsg msg )

                Routing.AddPosition ->
                    let
                        ( model, msg ) =
                            AddPositionPage.init
                    in
                    ( AddPosition model, Cmd.map AddPositionMsg msg )

                Routing.Home ->
                    case model.auth of
                        Nothing ->
                            let
                                ( model, msg ) =
                                    LoginPage.init
                            in
                            ( Login model, Cmd.map LoginMsg msg )

                        Just auth ->
                            let
                                ( model, msg ) =
                                    HomePage.init auth
                            in
                            ( Home model, Cmd.map HomeMsg msg )

                Routing.AddCompany ->
                    case model.auth of
                        Nothing ->
                            let
                                ( model, msg ) =
                                    LoginPage.init
                            in
                            ( Login model, Cmd.map LoginMsg msg )

                        Just user ->
                            let
                                ( model, msg ) =
                                    AddCompanyPage.init user Nothing
                            in
                            ( AddCompany model, Cmd.map AddCompanyMsg msg )

                Routing.EditCompany companyId ->
                    case model.auth of
                        Nothing ->
                            let
                                ( model, msg ) =
                                    LoginPage.init
                            in
                            ( Login model, Cmd.map LoginMsg msg )

                        Just user ->
                            let
                                ( model, msg ) =
                                    AddCompanyPage.init user (Just companyId)
                            in
                            ( AddCompany model, Cmd.map AddCompanyMsg msg )

                Routing.Company companyId ->
                    case model.auth of
                        Nothing ->
                            let
                                ( model, msg ) =
                                    LoginPage.init
                            in
                            ( Login model, Cmd.map LoginMsg msg )

                        Just user ->
                            let
                                ( model, msg ) =
                                    CompanyPage.init user.jwt (Just companyId)
                            in
                            ( Company model, Cmd.map CompanyMsg msg )

        Nothing ->
            ( NotFound NotFoundPage.initialModel, Cmd.none )


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    case ( msg, page ) of
        ( LocationChanged location, _ ) ->
            let
                ( newPage, msg ) =
                    pageFromLocation model location
            in
            ( { model | page = newPage }, msg )

        ( SetAuth auth, _ ) ->
            let
                cmd =
                    if model.auth /= Nothing && auth == Nothing then
                        Navigation.modifyUrl "#/login"
                    else
                        Cmd.none
            in
            ( { model | auth = auth }, cmd )

        ( LoginMsg subMsg, Login subMod ) ->
            let
                ( pageModel, msg, msgPage ) =
                    LoginPage.update subMsg subMod

                newModel =
                    case msgPage of
                        LoginPage.None ->
                            model

                        LoginPage.SetUser auth ->
                            { model | auth = Just auth }

                cmd =
                    case msgPage of
                        LoginPage.SetUser auth ->
                            Cmd.batch
                                [ Cmd.map LoginMsg msg, storeAuth auth, Navigation.modifyUrl "#/home" ]

                        LoginPage.None ->
                            Cmd.map LoginMsg msg
            in
            ( { newModel | page = Login pageModel }, cmd )

        ( RegistrationMsg subMsg, Registration subMod ) ->
            let
                ( pageModel, msg ) =
                    RegistrationPage.update subMsg subMod
            in
            ( { model | page = Registration pageModel }, Cmd.map RegistrationMsg msg )

        ( HomeMsg subMsg, Home subMod ) ->
            let
                ( pageModel, msg ) =
                    HomePage.update subMsg subMod
            in
            ( { model | page = Home pageModel }, Cmd.map HomeMsg msg )

        ( AddCompanyMsg subMsg, AddCompany subMod ) ->
            let
                ( pageModel, msg ) =
                    AddCompanyPage.update subMsg subMod
            in
            ( { model | page = AddCompany pageModel }, Cmd.map AddCompanyMsg msg )

        ( AddPositionMsg subMsg, AddPosition subMod ) ->
            let
                ( pageModel, msg ) =
                    AddPositionPage.update subMsg subMod
            in
            ( { model | page = AddPosition pageModel }, Cmd.map AddPositionMsg msg )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        user =
            Maybe.map .user model.auth
    in
    case model.page of
        NotFound submod ->
            NotFoundPage.view submod

        Login submod ->
            publicNavigation (LoginPage.view submod)
                |> Html.map LoginMsg

        Registration submod ->
            publicNavigation (RegistrationPage.view submod)
                |> Html.map RegistrationMsg

        Home submod ->
            privateNavigation user (HomePage.view submod)
                |> Html.map HomeMsg

        AddCompany submod ->
            privateNavigation user (AddCompanyPage.view submod)
                |> Html.map AddCompanyMsg

        Company submod ->
            privateNavigation user (CompanyPage.view submod)
                |> Html.map CompanyMsg

        AddPosition submod ->
            AddPositionPage.view submod
                |> Html.map AddPositionMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.page msg model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAuth authChange


main : Program Value Model Msg
main =
    Navigation.programWithFlags LocationChanged
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
