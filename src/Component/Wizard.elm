module Component.Wizard exposing (..)

import Html exposing (Html, div, ul, li, text)
import Html.Attributes exposing (class, style)

type alias Headlines = List String
type alias Content msg = List (Html msg)
type alias Buttons msg = Html msg

view : Int -> Headlines -> Content msg -> Buttons msg -> Html msg
view index headlines items buttons =
  div
    [ class "flex flex-column col-12 overflow-hidden" ]
    [ div
      [ class "wizard flex col-9 align-h-middle rounded" ]
      [ div
        [ class "wizard-steps-list col-3 py2 bg-blue" ]
        [ ul
          [ class "steps-list list list-reset" ]
          (List.indexedMap (mkSteplistItem index) headlines)
        ]
      , div
        [ class "wizard-content flex flex-column col-9" ]
        [ div
          [ class "wizard-list-slider"
          , style [("top", (toString (index * -100)) ++ "%")]
          ]
          (List.indexedMap (mkWizardItem index) items)
        , buttons
        ]
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

mkWizardItem : Int -> Int -> Html msg -> Html msg
mkWizardItem x y item =
  let
    classes = if x == y then "" else " hidden"
  in
    div
      [ class ("wizard-item" ++ classes) ]
      [ item ]
