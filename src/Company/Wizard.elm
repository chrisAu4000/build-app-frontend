module Company.Wizard exposing (..)

import Adress.Form as AdressForm exposing (validateAdressForm)
import Either exposing (Either)
import Company.Form as CompanyForm exposing (validateCompanyForm)
import Company.Model exposing (Company, CompanyId, empty, companyDefaultImg)
import Company.Request exposing (createCompany, fetchCompany)
import Component.ButtonBar exposing (ButtonL, ButtonR, Visibility(..), Ability(..), wizardButtons)
import Component.Completed as Completed exposing (CompleteMsg(..))
import Component.Wizard as Wizard_
import Html exposing (Html, Attribute, div, button, i, form, li, ul, text)
import RemoteData exposing (WebData)
import Auth.Model exposing (Token)
import Data.Validation as Validation exposing (Validation, (<*>))

type alias Model =
  { index : Int
  , buttonL : ButtonL Msg
  , buttonR : ButtonR Msg
  , companyForm : CompanyForm.Model
  , adressForm : AdressForm.Model
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


init : Token -> CompanyId -> (Model, Cmd Msg)
init token companyId =
  let
    cmd =
      case companyId of
        Nothing -> Cmd.none
        Just id -> fetchCompany token id
          |> Cmd.map OnAddedCompany
    buttonR =
      case companyId of
        Nothing -> Visible Disabled
        Just _ -> Hidden
  in
    ({ index = 0
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
  if i > 0 
  then Visible (Enabled Dec)
  else Hidden

buttonRight : Int -> List Bool -> Visibility Msg
buttonRight index models =
  if List.all (\x -> x == True) models
  then Visible (Enabled Submit)
  else models
    |> List.take (index + 2)
    |> List.foldr (&&) True
    |> (\bool ->
      if bool 
      then Visible (Enabled Inc)
      else Visible Disabled)

validateCompany : CompanyForm.Model -> AdressForm.Model -> Validation (List String) Company
validateCompany companyForm adressForm =
  Validation.pure (Company Nothing)
    <*> validateCompanyForm companyForm
    <*> (companyForm.logo
      -- |> Maybe.andThen Either.leftToMaybe
      |> Validation.pure)
    <*> validateAdressForm adressForm

adressFromCompany : Company -> AdressForm.Model -> AdressForm.Model
adressFromCompany company adress =
  { adress |
    street = company.adress.street
  , houseNr = company.adress.houseNr
  , postCode = company.adress.postCode
  , domicile = company.adress.domicile
  }

companyFormCompany : Company -> CompanyForm.Model -> CompanyForm.Model
companyFormCompany company companyForm =
  { companyForm |
    name = company.name
  , logo = company.logo
  , imgUrl = company.logo
    |> Maybe.andThen Either.rightToMaybe
    |> Maybe.map(\p -> "http://localhost:1337" ++ p)
  }

update : Msg -> Model -> Token -> (Model, Cmd Msg)
update msg model token =
  case msg of
    Inc ->
      ( { model 
        | index = model.index + 1
        , buttonL = buttonLeft (model.index + 1)
        , buttonR = buttonRight model.index [ model.companyForm.isValid, model.adressForm.isValid ]
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
          case validateCompany model.companyForm model.adressForm of
            Validation.Err _ -> Cmd.none
            Validation.Res company ->
              createCompany token company
              |> Cmd.map OnAddedCompany
        index = if model.index == 2 then 2 else model.index + 1
      in
        ( { model | company = RemoteData.Loading, index = index }, cmd )
    OnAddedCompany company ->
      let (buttonL, buttonR, companyForm, adressForm) =
        case company of
          RemoteData.Failure error ->
            (Visible (Enabled Dec), Hidden, model.companyForm, model.adressForm)
          RemoteData.Success company ->
            ( Hidden
            , Hidden
            , companyFormCompany company model.companyForm
            , adressFromCompany company model.adressForm
            )
          _ ->
            (Hidden, Hidden, model.companyForm, model.adressForm)
      in
        ( { model | 
            company = company
          , companyForm = companyForm
          , adressForm = adressForm
          , buttonL = buttonL
          , buttonR = buttonR
          }
        , Cmd.none
        )
    Reset -> init token Nothing
    CompanyFormMsg msg ->
      let
        (companyModel, companyMsg) = CompanyForm.update msg model.companyForm
        buttonR = if companyModel.isValid then Visible (Enabled Inc) else Visible Disabled
      in
        ({ model | companyForm = companyModel, buttonR = buttonR }, companyMsg |> Cmd.map CompanyFormMsg)
    AdressFormMsg msg ->
      let
        (adressModel, adressMsg) = AdressForm.update msg model.adressForm
        buttonR = if adressModel.isValid then Visible (Enabled Submit) else Visible Disabled
      in
        ({ model | adressForm = adressModel, buttonR = buttonR }, adressMsg |> Cmd.map AdressFormMsg)


view : Model -> Html Msg
view model =
  Wizard_.view
    model.index
    ["Company", "Adress", "Complete"]
    [ (CompanyForm.view model.companyForm) |> Html.map CompanyFormMsg
    , (AdressForm.view model.adressForm) |> Html.map AdressFormMsg
    , (Completed.view (CompleteMsg Submit, CompleteMsg Reset) model.company)
    ]
    (wizardButtons (model.buttonL, model.buttonR))