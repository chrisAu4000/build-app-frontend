module Wizard.Model exposing (..)

import Component.Button exposing (submitBtnNotAsked)
import Component.Icon as Icon exposing (icon)
import Company.Model exposing (Company)
import Html exposing (Html, Attribute, button, div, form, i, li, ul, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onSubmit)
import RemoteData exposing (WebData)

type alias IsValid = Bool
type alias Step msg =
  { lable : String
  , view :Html msg
  -- , model : model
  -- , update : msg -> model -> (model, Cmd msg)
  }

type alias Model msg =
  { index : Int
  , steps : List (Step msg)
  }

type Msg msg
  = Inc
  | Dec
  | StepMsg msg

init : List (Step msg) -> Model msg
init steps =
  { index = 0
  , steps = steps
  }

-- updateCurrent : msg -> Int -> Int -> Step msg -> Step msg
-- updateCurrent msg x y step = 
--   if x == y 
--   then step 
--   else step

-- update : Msg msg -> Model Msg -> (Model Msg, Cmd (Msg msg))
-- update msg model =
--   case msg of
--     Inc -> ({ model | index = model.index + 1 }, Cmd.none)
--     Dec -> ({ model | index = model.index - 1 }, Cmd.none)
--     StepMsg subMsg -> 
--       let
--         newSteps = List.indexedMap (updateCurrent subMsg model.index) model.steps
--       in
--         (model, Cmd.none)

view : Model msg -> Html Msg
view model
  = let
    wizardHeight =  (List.length model.steps) * 7
    heightStr i =
      "calc(" ++ (toString i) ++ "*(" ++ (toString wizardHeight ++ "rem") ++ " + 89px))"
    currentStep = get model.index 0 model.steps
    
  in
    div
      [ class "flex flex-column col-12 overflow-hidden"]
      [ div
        [ class "wizard flex col-9 align-h-middle rounded"
        , style [ ( "height", heightStr 1 ) ]
        ]
        [
        -- [ div
        --   [ class "wizard-steps-list col-3 py2 bg-blue" ]
        --   [ ul
        --     [ class "list list-reset steps-list" ]
        --     (List.indexedMap (mkSteplistItem model.index) model.steps)
        --   ]
        -- , div
        --   [ class "wizard-content flex flex-column col-9" ]
        --   [ div
        --     [ class "wizard-list-slider"
        --     , style [ ( "margin-top", heightStr -model.index ) ]
        --     ]
        --     (List.indexedMap (mkWizardItem model.index) model.steps)
        --   , (wizardButtons (stepToButtons currentStep) (model.index == (List.length model.steps) - 2))
        --   ]
        ]
      ]

type alias ActiveItem = Int
type alias Index = Int

mkSteplistItem : ActiveItem -> Index -> String -> Html msg
mkSteplistItem x y step =
  let
    item =
      case x == y of
        True -> Active step
        False ->
          case x < y of
            True -> Done step
            False -> NotAsked step
  in
    steplistItem item

type alias IsActive = Bool
type alias IsDone = Bool
type alias Text = String

type SteplistItem a
  = Active a
  | Done a
  | NotAsked a
steplistItem :SteplistItem String -> Html msg
steplistItem steplistItem =
  let
    (classes, step) =
      case steplistItem of
        Active s -> ("active", s)
        Done s -> ("done", s)
        NotAsked s -> ("", s)
  in
    li
      [ class ("ml1 mb2 px1 steps-list-item " ++ classes) ]
      [ div
        [ class "table-cell align-middle"]
        [ text step ]
      ]

wizardItem : IsActive -> Html msg -> Html msg
wizardItem isActive child =
  let
    classes = if isActive then "" else " hidden"
  in
    div
      [ class ("wizard-item" ++ classes) ]
      [ child ]

mkWizardItem : ActiveItem -> Index -> Step Msg -> Html Msg
mkWizardItem x y step =
  let
    child = step.view
  in
    wizardItem (x == y) child

type Ability msg
  = Enabled msg
  | Disabled

type Visibility msg
  = Visible (Ability msg)
  | Hidden

type alias ButtonL msg = Visibility msg
type alias ButtonR msg = Visibility msg
type alias IsComplete = Bool

wizardButtons : (ButtonL msg, ButtonR msg) -> Html msg
wizardButtons (bl, br) =
  let
    toClasses : Visibility msg -> (String, List (Attribute msg))
    toClasses b =
      case b of
        Hidden -> (" hidden disabled", [])
        Visible bv ->
          case bv of
            Disabled -> (" disabled", [])
            Enabled msg -> ("", [onClick msg])
    (leftButtonClasses, leftButtonAction) = toClasses bl
    (rightButtonClasses, rightButtonAction) = toClasses br
  in
    div
      [ class "wizard-button-bar flex relative height-0"]
      [ button 
        ([ class ("btn circle wizard-button blue" ++ leftButtonClasses) ] ++ leftButtonAction)
        [ i
          [ class "icon fa fa-arrow-circle-o-left fa-4x"]
          []
        ]
      , button
        ([ class ("btn circle wizard-button right green" ++ rightButtonClasses) ] ++ rightButtonAction)
        [ i
          [ class "icon fa fa-check-circle-o fa-4x"]
          []
        ]
      ]

type StepState a
  = First IsValid
  | Middle IsValid
  | Last (WebData a)

stepToButtons : onBack -> 
                onForward -> 
                Maybe (StepState a) -> 
                (ButtonL onBack, ButtonR onForward)
stepToButtons backMsg forwardMsg state =
  case state of
    Nothing -> (Hidden, Hidden)
    Just button ->
      case button of
        First valid -> 
          case valid of
            False -> (Hidden, Visible Disabled)
            True -> (Hidden, Visible (Enabled forwardMsg))
        Middle valid ->
          case valid of
            False -> (Visible (Enabled backMsg), Visible Disabled)
            True -> (Visible (Enabled backMsg), Visible (Enabled forwardMsg))
        Last valid ->
          case valid of
            RemoteData.Loading -> (Hidden, Hidden)
            RemoteData.Failure _ -> (Visible (Enabled backMsg), Hidden)
            RemoteData.Success _ -> (Hidden, Hidden)
            RemoteData.NotAsked -> (Hidden, Hidden)

get : Int -> Int -> List a -> Maybe a
get i j list =
  let
    tail = Maybe.withDefault [] (List.tail list)
  in
    case List.head list of
      Nothing -> Nothing
      Just a -> 
        if i == j 
        then Just a 
        else get i (j + 1) tail

type CompleteMsg msg = CompleteMsg msg

completeView : (CompleteMsg msg, CompleteMsg msg) -> WebData Company -> Html msg
completeView (onRetry, onReset) company =
  let
    retry = case onRetry of CompleteMsg msg -> msg
    reset = case onReset of CompleteMsg msg -> msg
    (iconClass, message, button, action) =
      case company of
        RemoteData.Loading ->
          (icon Icon.Loading, "Saving...", Nothing, [])
        RemoteData.Failure e ->
          ( ("red " ++ icon Icon.Failure)
          , "Something went wrong"
          , (Just (submitBtnNotAsked "Try Again"))
          , [onSubmit retry]
          )
        RemoteData.Success c ->
          ( ("green " ++ icon Icon.Success)
          , "Company Saved"
          , (Just (submitBtnNotAsked "Add another company"))
          , [onSubmit reset]
          )
        RemoteData.NotAsked ->
          ("", "", Nothing, [])
  in
    form
      ([ class "wizard-complete flex" ] ++ action)
      [ div
        [ class "align-v-middle align-h-middle center flex flex-column"]
        [ i
          [ class ("fa-4x center " ++ iconClass) ]
          [ ]
        , div
          [ class "mb1" ]
          [ text message ]
        , Maybe.withDefault (div [] []) button
        ]
      ]
