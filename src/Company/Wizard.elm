module Company.Wizard exposing (..)

import Adress.Form as AdressForm
import Adress.Model exposing (Adress)
import Adress.Validation exposing (validateAdress, validateDomicile, validateHouseNr, validatePostCode, validateStreet)
import Auth.Model exposing (Token)
import Company.Form as CompanyForm
import Company.Model exposing (Company, CompanyId, companyDefaultImg, empty)
import Company.Request exposing (createCompany, fetchCompany)
import Company.Validation as CompanyForm exposing (validateCompany, validateCompanyName)
import Component.ButtonBar exposing (Ability(..), ButtonL, ButtonR, Visibility(..), wizardButtons)
import Component.Completed as Completed exposing (CompleteMsg(..))
import Component.Wizard as Wizard_
import Data.ValidationInput as ValidationInput exposing (ValidationInput)
import Either exposing (Either)
import Html exposing (Attribute, Html, button, div, form, i, li, text, ul)
import RemoteData exposing (WebData)


type alias Model =
    { index : Int
    , buttonL : ButtonL Msg
    , buttonR : ButtonR Msg
    , companyForm : CompanyForm.Model
    , adressForm : Adress
    , company : WebData Company
    }


type Msg
    = Inc
    | Dec
    | Submit
    | Reset
    | OnAddedCompany (WebData Company)
    | CompanyFormMsg CompanyForm.Msg
    | AdressFormMsg AdressForm.Msg


init : Token -> CompanyId -> ( Model, Cmd Msg )
init token companyId =
    let
        cmd =
            case companyId of
                Nothing ->
                    Cmd.none

                Just id ->
                    fetchCompany token id
                        |> Cmd.map OnAddedCompany

        buttonR =
            case companyId of
                Nothing ->
                    Visible Disabled

                Just _ ->
                    Hidden
    in
    ( { index = 0
      , buttonL = Hidden
      , buttonR = buttonR
      , companyForm = CompanyForm.init
      , adressForm = AdressForm.init
      , company = RemoteData.NotAsked
      }
    , cmd
    )


buttonLeft : Int -> Visibility Msg
buttonLeft i =
    if i > 0 then
        Visible (Enabled Dec)
    else
        Hidden


buttonRight : Int -> List Bool -> Visibility Msg
buttonRight index models =
    let
        _ =
            Debug.log "index" (toString index)

        _ =
            Debug.log "mods" (toString models)
    in
    if List.all (\x -> x == True) models then
        Visible (Enabled Submit)
    else
        models
            |> List.take (index + 1)
            |> List.foldr (&&) True
            |> (\bool ->
                    if bool then
                        Visible (Enabled Inc)
                    else
                        Visible Disabled
               )


adressFromCompany : Company -> Adress -> Adress
adressFromCompany company adress =
    { adress
        | street = validateStreet company.adress.street
        , houseNr = validateHouseNr company.adress.houseNr
        , postCode = validatePostCode company.adress.postCode
        , domicile = validateDomicile company.adress.domicile
    }


companyFormCompany : Company -> CompanyForm.Model -> CompanyForm.Model
companyFormCompany company companyForm =
    { companyForm
        | name = validateCompanyName company.name
        , logo = company.logo
        , imgUrl =
            company.logo
                |> Maybe.andThen Either.rightToMaybe
                |> Maybe.map (\p -> "http://localhost:1337" ++ p)
    }


update : Msg -> Model -> Token -> ( Model, Cmd Msg )
update msg model token =
    case msg of
        Inc ->
            let
                isValidCompany =
                    case validateCompanyName model.companyForm.name of
                        ValidationInput.Err _ _ ->
                            False

                        ValidationInput.Ok _ ->
                            True

                isValidAdress =
                    case validateAdress model.adressForm of
                        ValidationInput.Err _ _ ->
                            False

                        ValidationInput.Ok _ ->
                            True
            in
            ( { model
                | index = model.index + 1
                , buttonL = buttonLeft (model.index + 1)
                , buttonR = buttonRight model.index [ isValidCompany, isValidAdress ]
              }
            , Cmd.none
            )

        Dec ->
            ( { model
                | index = model.index - 1
                , buttonL = buttonLeft (model.index - 1)
                , buttonR = Visible (Enabled Inc)
              }
            , Cmd.none
            )

        Submit ->
            let
                cmd =
                    case validateCompany model.companyForm.name model.companyForm.logo model.adressForm of
                        ValidationInput.Err _ _ ->
                            Cmd.none

                        ValidationInput.Ok company ->
                            createCompany token company
                                |> Cmd.map OnAddedCompany

                index =
                    if model.index == 2 then
                        2
                    else
                        model.index + 1
            in
            ( { model | company = RemoteData.Loading, index = index }, cmd )

        OnAddedCompany company ->
            let
                ( buttonL, buttonR, companyForm, adressForm ) =
                    case company of
                        RemoteData.Failure error ->
                            ( Visible (Enabled Dec), Hidden, model.companyForm, model.adressForm )

                        RemoteData.Success company ->
                            ( Hidden
                            , Hidden
                            , companyFormCompany company model.companyForm
                            , adressFromCompany company model.adressForm
                            )

                        _ ->
                            ( Hidden, Hidden, model.companyForm, model.adressForm )
            in
            ( { model
                | company = company
                , companyForm = companyForm
                , adressForm = adressForm
                , buttonL = buttonL
                , buttonR = buttonR
              }
            , Cmd.none
            )

        Reset ->
            init token Nothing

        CompanyFormMsg msg ->
            let
                ( companyModel, companyMsg ) =
                    CompanyForm.update msg model.companyForm

                isValidCompany =
                    case validateCompanyName companyModel.name of
                        ValidationInput.Err _ _ ->
                            False

                        ValidationInput.Ok _ ->
                            True

                buttonR =
                    buttonRight 0 [ isValidCompany, False ]

                -- if companyModel.isValid then Visible (Enabled Inc) else Visible Disabled
            in
            ( { model | companyForm = companyModel, buttonR = buttonR }, companyMsg |> Cmd.map CompanyFormMsg )

        AdressFormMsg msg ->
            let
                ( adressModel, adressMsg ) =
                    AdressForm.update msg model.adressForm

                isValidAdress =
                    case validateAdress adressModel of
                        ValidationInput.Err _ _ ->
                            False

                        ValidationInput.Ok _ ->
                            True

                buttonR =
                    buttonRight 1 [ True, isValidAdress ]
            in
            ( { model | adressForm = adressModel, buttonR = buttonR }, adressMsg |> Cmd.map AdressFormMsg )


view : Model -> Html Msg
view model =
    Wizard_.view
        model.index
        [ "Company", "Adress", "Complete" ]
        [ CompanyForm.view model.companyForm |> Html.map CompanyFormMsg
        , AdressForm.view model.adressForm |> Html.map AdressFormMsg
        , Completed.view ( CompleteMsg Submit, CompleteMsg Reset ) model.company
        ]
        (wizardButtons ( model.buttonL, model.buttonR ))
