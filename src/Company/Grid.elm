module Company.Grid exposing (..)

import Auth.Model exposing (Token)
import Company.Model exposing (Company, CompanyId)
import Company.Request exposing (fetchCompanies, removeCompany)
import Component.Button exposing (deleteBtn, editBtn, listHeaderBtn)
import Component.Icon as Icon
import Component.Loader exposing (load)
import Data.ValidationInput exposing (get)
import Either exposing (rightToMaybe)
import Html exposing (Html, a, br, button, div, i, img, text)
import Html.Attributes exposing (class, href, src, style)
import Html.Events exposing (onClick)
import Navigation
import RemoteData exposing (WebData)


type alias DisclaymerShown =
    Bool


type alias Companies =
    WebData (List ( DisclaymerShown, Company ))


type alias Model =
    { token : Token
    , companies : Companies
    }


type Msg
    = OnFetch Companies
    | ShowRemoveDisclaymer CompanyId
    | RemoveDisclaymers
    | Remove CompanyId
    | OnRemove (WebData Company)
    | GotoEditCompany CompanyId


init : Token -> ( Model, Cmd Msg )
init token =
    ( { token = token, companies = RemoteData.Loading }
    , fetchCompanies token
        |> Cmd.map (RemoteData.map (List.map (\c -> ( False, c ))))
        |> Cmd.map OnFetch
    )


remove : CompanyId -> Companies -> Companies
remove id =
    RemoteData.map (List.filter (not << (==) id << .id << Tuple.second))


companiesMap : (( Bool, Company ) -> ( Bool, Company )) -> Companies -> Companies
companiesMap f =
    RemoteData.map (List.map f)


isEqual : a -> a -> Bool
isEqual a b =
    a == b


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnFetch companies ->
            ( { model | companies = companies }, Cmd.none )

        ShowRemoveDisclaymer companyId ->
            case ( companyId, model.companies ) of
                ( Just companyId, RemoteData.Success companies ) ->
                    let
                        equalIds =
                            isEqual companyId

                        newCompanies =
                            List.map
                                (\( _, com ) ->
                                    case com.id of
                                        Nothing ->
                                            ( False, com )

                                        Just companyId ->
                                            if equalIds companyId then
                                                ( True, com )
                                            else
                                                ( False, com )
                                )
                                companies
                    in
                    ( { model | companies = RemoteData.Success newCompanies }, Cmd.none )

                ( _, RemoteData.Failure error ) ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RemoveDisclaymers ->
            let
                newCompanies =
                    model.companies
                        |> companiesMap (Tuple.mapFirst (always False))
            in
            ( { model | companies = newCompanies }, Cmd.none )

        Remove maybeId ->
            case maybeId of
                Nothing ->
                    ( model, Cmd.none )

                Just id ->
                    ( model, removeCompany model.token id |> Cmd.map OnRemove )

        OnRemove company ->
            case company of
                RemoteData.Loading ->
                    ( model, Cmd.none )

                RemoteData.Failure error ->
                    let
                        _ =
                            Debug.log "ERROR" (toString error)
                    in
                    ( model, Cmd.none )

                RemoteData.Success company ->
                    let
                        _ =
                            Debug.log "SUCCES" (toString company)
                    in
                    ( { model | companies = remove company.id model.companies }, Cmd.none )

                RemoteData.NotAsked ->
                    ( model, Cmd.none )

        GotoEditCompany companyId ->
            case companyId of
                Nothing ->
                    ( model, Cmd.none )

                Just id ->
                    ( model, Navigation.newUrl ("#/companyEdit/" ++ id) )


removeOverlay : Company -> List (Html Msg)
removeOverlay company =
    [ div
        [ class "remove-disclaymer p1" ]
        [ div
            [ class "remove-disclaymer--text center bold mt2" ]
            [ text "Do you realy want to remove"
            , br [] []
            , div
                [ class "underline" ]
                [ text (get company.name ++ "?") ]
            ]
        , div
            [ class "clearfix remove-disclaymer--buttons p1" ]
            [ button
                [ class "btn bg-green"
                , onClick RemoveDisclaymers
                ]
                [ text "CANCEL" ]
            , button
                [ class "btn bg-red"
                , onClick (Remove company.id)
                ]
                [ text "REMOVE" ]
            ]
        ]
    ]


gridItem : ( Bool, Company ) -> Html Msg
gridItem ( showsDisclaymer, company ) =
    let
        imgSrc =
            company.logo
                |> Maybe.andThen rightToMaybe
                |> Maybe.withDefault "/img/company-placeholder.png"
                |> (\path -> "http://localhost:1337" ++ path)

        companyEditUrl =
            company.id
                |> Maybe.map (\id -> "#/company/" ++ id)
                |> Maybe.withDefault ""

        companyRef =
            company.id
                |> Maybe.map (\id -> [ href ("#/company/" ++ id) ])
                |> Maybe.withDefault []

        disclaymer =
            if showsDisclaymer then
                " show-disclaymer"
            else
                ""

        overlay =
            if showsDisclaymer then
                removeOverlay company
            else
                []
    in
    div
        [ class "relative overflow-hidden company grid-item shadow sm-col sm-col-12 md-col md-col-4 lg-col lg-col-3" ]
        [ div
            [ class "company-img-wrapper overflow-hidden flex" ]
            ([ img
                [ class "company-img mx-auto align-v-middle"
                , src imgSrc
                ]
                []
             ]
                ++ overlay
            )
        , div
            [ class ("company-button-bar overflow-hidden height-0" ++ disclaymer) ]
            [ button
                [ class "btn circle border"
                , onClick (GotoEditCompany company.id)
                ]
                [ i
                    [ class "icon fa fa-pencil-square-o" ]
                    []
                ]
            , button
                [ class "btn circle border"
                , onClick (ShowRemoveDisclaymer company.id)
                ]
                [ i
                    [ class "icon fa fa-trash-o" ]
                    []
                ]
            ]
        , div
            [ class "company-name block h3 center bold py1 border-top" ]
            [ a
                companyRef
                [ text (get company.name) ]
            ]
        ]



-- [ button
--         ([ class ("btn circle wizard-button blue" ++ leftButtonClasses) ] ++ leftButtonAction)
--         [ i
--           [ class "icon fa fa-arrow-circle-o-left fa-4x"]
--           []
--         ]
--       , button
--         ([ class ("btn circle wizard-button right green" ++ rightButtonClasses) ] ++ rightButtonAction)
--         [ i
--           [ class "icon fa fa-check-circle-o fa-4x"]
--           []
--         ]
--       ]


grid : Companies -> List (Html Msg)
grid companies =
    case companies of
        RemoteData.Loading ->
            [ load ]

        RemoteData.Failure err ->
            [ text (toString err) ]

        RemoteData.Success cms ->
            List.map gridItem cms

        RemoteData.NotAsked ->
            [ text "" ]


view : Model -> Html Msg
view model =
    div
        [ class "clearfix m2" ]
        [ div
            [ class "clearfix button-bar" ]
            [ listHeaderBtn ( Icon.Plus, "#/company" ) ]
        , div
            [ class "max-width-5 mx-auto" ]
            [ div
                [ class "clearfix grid" ]
                (grid model.companies)
            ]
        ]
