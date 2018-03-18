module Company.Wizard exposing (..)

import Adress.Form as AdressForm exposing (validateAdressForm)
import Company.Form as CompanyForm exposing (validateCompanyForm)
import Company.Model exposing (Company, empty)
import Company.Request exposing (createCompany)
import Component.ButtonBar exposing (ButtonL, ButtonR, Visibility(..), Ability(..), wizardButtons)
import Component.Wizard as Wizard_
import Either exposing (Either(..))
import Html exposing (Html, Attribute, div, button, i, form, li, ul, text)
import RemoteData exposing (WebData)
import Auth.Model exposing (Token)
import Validation exposing (Validation, (<*>))
import Wizard.Model as Wizard exposing (StepState(..), CompleteMsg(..), stepToButtons, mkSteplistItem, completeView)

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

init : Model
init =
  { index = 0
  , buttonL = Hidden
  , buttonR = Visible Disabled
  , companyForm = CompanyForm.init
  , adressForm = AdressForm.init
  , company = RemoteData.NotAsked
  }

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
    <*> validateAdressForm adressForm

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
      let (buttonL, buttonR) =
        case company of
          RemoteData.Failure _ -> (Visible (Enabled Dec), Hidden)
          RemoteData.Success _ -> (Hidden, Hidden)
          _ -> (Hidden, Hidden)
      in
        ({ model | company = company, buttonL = buttonL, buttonR = buttonR }, Cmd.none)
    Reset -> (init, Cmd.none)
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
    , (completeView (CompleteMsg Submit, CompleteMsg Reset) model.company)
    ]
    (wizardButtons (model.buttonL, model.buttonR))