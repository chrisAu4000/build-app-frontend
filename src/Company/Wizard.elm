module Company.Wizard exposing (..)

import Adress.Form as AdressForm exposing (validateAdressForm)
import Adress.Model exposing (Adress)
import Company.Form as CompanyForm exposing (validateCompanyForm)
import Company.Model exposing (Company, empty)
import Company.Request exposing (createCompany)
import Html exposing (Html, Attribute, div, button, i, form, li, ul, text)
import Html.Attributes exposing (class, style)
import RemoteData exposing (WebData)
import User.Model exposing (Token)
import Validation exposing (Validation, (<*>))
import Wizard.Model exposing (StepState(..), CompleteMsg(..), stepToButtons, wizardButtons, mkSteplistItem, completeView)

type alias Model =
  { index : Int
  , companyDataForm : CompanyForm.Model
  , companyDataFormError : Validation (List String) String
  , adressForm : AdressForm.Model
  , adressFormError : Validation (List String) Adress
  , company : WebData Company
  , steps : List (Step Msg)
  }

type Msg
  = WizardInc
  | WizardDec
  | WizardNoOp
  | Submit
  | OnAddedCompany (WebData Company)
  | CompanyDataFormMsg CompanyForm.Msg
  | AdressFormMsg AdressForm.Msg
  | Reset


type Form
  = CompanyForm (Validation (List String) String)
  | AdressForm (Validation (List String) Adress)
  | CompleteForm (WebData Company)

type alias Step msg = (String, Html msg)

type alias ActiveIndex = Int
type alias Index = Int
type alias Complete = Bool

getSteps : Model -> List (Step Msg)
getSteps model =
  let
    companyForm =
      CompanyForm.view model.companyDataForm
        |> Html.map CompanyDataFormMsg
    adressForm =
      AdressForm.view model.adressForm
        |> Html.map AdressFormMsg
    complete =
      completeView (CompleteMsg Submit, CompleteMsg Reset) model.company
  in
    [ ("Company Name", companyForm)
    , ("Adress", adressForm)
    , ("Complete", complete)
    ]

init : Model
init =
  let
    companyForm =
      Html.map CompanyDataFormMsg (CompanyForm.view CompanyForm.init)
    adressForm =
      Html.map AdressFormMsg (AdressForm.view AdressForm.init)
    complete =
      completeView (CompleteMsg Submit, CompleteMsg Reset) RemoteData.NotAsked
  in
    { index = 0
    , companyDataForm = CompanyForm.init
    , companyDataFormError = Validation.Err []
    , adressForm = AdressForm.init
    , adressFormError = Validation.Err []
    , company = RemoteData.NotAsked
    , steps =
      [ ("Company Name", companyForm)
      , ("Adress", adressForm)
      , ("Complete", complete)
      ]
    }

update : Msg -> Model -> Token -> (Model, Cmd Msg)
update msg model token =
  case msg of
    WizardInc -> ({ model | index = model.index + 1 }, Cmd.none)
    WizardDec -> ({ model | index = model.index - 1 }, Cmd.none)
    WizardNoOp -> (model, Cmd.none)
    Submit ->
      let
        company = Validation.pure Company
          <*> validateCompanyForm model.companyDataForm
          <*> validateAdressForm model.adressForm
        cmd =
          case company of
            Validation.Err _ -> Cmd.none
            Validation.Res c ->
              createCompany token c
              |> Cmd.map OnAddedCompany
      in
        ( { model | index = (List.length model.steps) - 1, company = RemoteData.Loading } , cmd )
    OnAddedCompany company ->
      ({ model | company = company }, Cmd.none)
    CompanyDataFormMsg formMsg ->
      let
        (form, foMsg) = CompanyForm.update formMsg model.companyDataForm
        validation = validateCompanyForm form
      in
        ({ model | companyDataForm = form, companyDataFormError = validation }, Cmd.map CompanyDataFormMsg foMsg)
    AdressFormMsg formMsg ->
      let
        (form, foMsg) = AdressForm.update formMsg model.adressForm
        validation = validateAdressForm form
      in
        ({ model | adressForm = form, adressFormError = validation }, Cmd.map AdressFormMsg foMsg)
    Reset -> (init, Cmd.none)

wizardItem : Bool -> Html Msg -> Html Msg
wizardItem isActive child =
  let
    classes = if isActive then "" else " hidden"
  in
    div
      [ class ("wizard-item" ++ classes) ]
      [ child ]

mkWizardItem : ActiveIndex -> Index -> Step Msg -> Html Msg
mkWizardItem x y step =
  let
    child = Tuple.second step
  in
    wizardItem (x == y) child

view : Model -> Html Msg
view model =
  let
    steps = getSteps model
    wizardHeight =  (List.length steps) * 7
    heightStr i =
      "calc(" ++ (toString i) ++ "*(" ++ (toString wizardHeight ++ "rem") ++ " + 89px))"
    (stepState, stepAction) =
      case (model.index, (model.index == (List.length steps) - 1)) of
       (0, False) -> (Just (First model.companyDataForm.isValid), WizardInc)
       (1, False) -> (Just (Middle model.adressForm.isValid), Submit)
       (_, True) -> (Just (Last model.company), WizardNoOp)
       (_,_) -> (Nothing, WizardNoOp)
  in
    div
      [ class "flex flex-column col-12 overflow-hidden"]
      [ div
        [ class "wizard flex col-9 align-h-middle rounded"
        , style [ ( "height", heightStr 1 ) ]
        ]
        [ div
          [ class "wizard-steps-list col-3 py2 bg-blue" ]
          [ ul
            [ class "list list-reset steps-list" ]
            (List.indexedMap (mkSteplistItem model.index) (List.map Tuple.first steps))
          ]
        , div
          [ class "wizard-content flex flex-column col-9" ]
          [ div
            [ class "wizard-list-slider"
            , style [ ( "margin-top", heightStr -model.index ) ]
            ]
            ( (List.indexedMap (mkWizardItem model.index) steps)
            ++ [ completeView (CompleteMsg Submit, CompleteMsg Reset) model.company ]
            )
          , (wizardButtons (stepToButtons WizardDec stepAction stepState))
          ]
        ]
      ]
