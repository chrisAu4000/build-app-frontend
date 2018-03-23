module Company.Form exposing (..)

import Adress.Form as AdressForm
import Auth.Model exposing (Token)
import Company.Model as Company exposing (Company, CompanyId, ImageSrc, Logo)
import Company.Request exposing (createCompany, fetchCompany)
import Company.Validation exposing (validateCompany)
import Component.Button exposing (submitBtnFailure, submitBtnLoading, submitBtnNotAsked, submitBtnSuccess)
import Component.Form exposing (centeredForm)
import Component.Input exposing (labeledFileInput, labeledTextInputValidation)
import Data.ValidationInput as ValidationInput exposing ((<*>), ValidationInput)
import Either exposing (Either(..))
import FileReader exposing (NativeFile)
import Html exposing (Attribute, Html, div, form, img, input, text)
import Html.Attributes exposing (accept, class, id, src, type_, value)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)
import Task


type alias Request =
    WebData Company


type alias Model =
    { company : Company
    , request : Request
    , imgSrc : Maybe ImageSrc
    , token : Token
    }


updateCompany : (Company -> Company) -> Model -> Model
updateCompany f model =
    ((\c -> { model | company = c }) << f << .company) model


type Msg
    = InputCompanyName String
    | InputCompanyLogo (List NativeFile)
    | OnDataUrl (Result FileReader.Error FileReader.FileContentDataUrl)
    | OnFetchCompany Request
    | AdressFormMsg AdressForm.Msg
    | Submit Company
    | OnSubmit Request


init : Token -> CompanyId -> ( Model, Cmd Msg )
init token companyId =
    let
        cmd =
            case companyId of
                Nothing ->
                    Cmd.none

                Just id ->
                    fetchCompany token id
                        |> Cmd.map OnFetchCompany
    in
    ( { company = Company.empty
      , request = NotAsked
      , imgSrc = Nothing
      , token = token
      }
    , cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputCompanyName name ->
            ( updateCompany (\c -> { c | name = ValidationInput.Ok name }) model, Cmd.none )

        InputCompanyLogo files ->
            case files of
                [ file ] ->
                    ( updateCompany (\c -> { c | logo = Just (Left file) }) model
                    , FileReader.readAsDataUrl file.blob
                        |> Task.attempt OnDataUrl
                    )

                _ ->
                    ( model, Cmd.none )

        OnDataUrl url ->
            case
                Result.mapError FileReader.prettyPrint url
                    |> Result.andThen (Decode.decodeValue Decode.string)
            of
                Err err ->
                    let
                        _ =
                            Debug.log "files" (toString err)
                    in
                    ( model, Cmd.none )

                Ok urlS ->
                    ( { model | imgSrc = Just urlS }, Cmd.none )

        OnFetchCompany company ->
            case company of
                RemoteData.NotAsked ->
                    ( model, Cmd.none )

                RemoteData.Loading ->
                    ( model, Cmd.none )

                RemoteData.Failure err ->
                    ( model, Cmd.none )

                RemoteData.Success company ->
                    ( { model | company = company }, Cmd.none )

        AdressFormMsg msg ->
            let
                ( subModel, subMsg ) =
                    AdressForm.update msg model.company.adress
            in
            ( updateCompany (\c -> { c | adress = subModel }) model, subMsg |> Cmd.map AdressFormMsg )

        Submit company ->
            ( updateCompany validateCompany model, createCompany model.token company |> Cmd.map OnSubmit )

        OnSubmit request ->
            ( { model | request = request, company = Company.empty }, Cmd.none )


emptyImage : Html Msg
emptyImage =
    img [ class "company-logo__empty" ] []


image : String -> Html Msg
image url =
    div
        [ class "flex-align-middle" ]
        [ img
            [ class "company-logo"
            , src url
            ]
            []
        ]


renderImg : Logo -> Maybe ImageSrc -> Html Msg
renderImg logo imgSrc =
    case imgSrc of
        Just url ->
            image url

        Nothing ->
            case logo of
                Nothing ->
                    emptyImage

                Just ima ->
                    case ima of
                        Left _ ->
                            emptyImage

                        Right url ->
                            image url


renderSubmitButton : Request -> Html Msg
renderSubmitButton request =
    case request of
        NotAsked ->
            submitBtnNotAsked "Add a Company"

        Loading ->
            submitBtnLoading ""

        Failure error ->
            submitBtnLoading (toString error)

        Success _ ->
            submitBtnSuccess "Company was added"


view : Model -> Html Msg
view model =
    centeredForm
        [ class "flex flex-column"
        , onSubmit (Submit model.company)
        ]
        [ form
            [ class "company-name-form mb1" ]
            [ div
                [ class "col col-12" ]
                [ div
                    [ class "image-input-wrapper mb2 right col-6 flex flex-column" ]
                    [ renderImg model.company.logo model.imgSrc
                    , labeledFileInput "company-logo"
                        "Company Logo"
                        [ class "image-input p0 m0 border-none"
                        , accept ".jpg, .jpeg, .png"
                        , FileReader.onFileChange InputCompanyLogo
                        ]
                    ]
                ]
            , div
                [ class "col col-12" ]
                [ labeledTextInputValidation
                    InputCompanyName
                    "Company Name"
                    model.company.name
                ]
            ]
        , AdressForm.view model.company.adress |> Html.map AdressFormMsg
        , renderSubmitButton model.request
        ]
