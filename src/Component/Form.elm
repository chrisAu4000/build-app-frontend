module Component.Form exposing (..)

import Html exposing (Attribute, Html, div, input, label, li, form, text, textarea, ul)
import Html.Attributes exposing (class, id, for, type_, required, style, value)
import Validation exposing (Validation)

type alias Label = String

centeredForm : List (Attribute msg) -> List (Html msg) -> Html msg
centeredForm attrs children =
  let
    attributes = attrs ++ [ class "clearfix m1 form" ]
  in
    div 
      [ class "clearfix align-h-middle col col-6" ]
      [ form attributes children ]

textInput : List (Attribute msg) -> List (Html msg) -> Html msg
textInput attrs =
  input 
    ( [ type_ "text", class "input not-rounded" ] ++ attrs )

passwordInput : List (Attribute msg) -> List (Html msg) -> Html msg
passwordInput attrs =
  input 
    ( [ type_ "password", class "input not-rounded" ] ++ attrs )

fileInput : List (Attribute msg) -> List (Html msg) -> Html msg
fileInput attrs =
  input
    ( [ type_ "file", class "input not-rounded" ] ++ attrs)

type alias LabeledInput msg =
  Label -> List (Attribute msg) -> List (Html msg) -> Html msg
type alias Id = String

labeledTextInput : LabeledInput msg
labeledTextInput label_ attrs _ =
  div
    [ class "validation-input" ]
    [ label [] [ text label_ ]
    , textInput attrs []
    ]

labeledFileInput : Id -> LabeledInput msg
labeledFileInput id_ label_ attrs _ =
  div
    [ class "btn bg-blue white text-middle"]
    [ fileInput (attrs ++ [ id id_ ]) []
    , label [ for id_ ] [ text label_ ]
    ]

labeledTextInputValidation : Validation (List String) String -> LabeledInput msg
labeledTextInputValidation result label_ attrs _ =
  case result of
    Validation.Err msgs ->
      -- errorMessage (\attr -> labeledTextInput label_ (attrs ++ attr) []) msgs
      div
        [ class "relative" ]
        [ labeledTextInput label_ (attrs ++ [ class "border-red" ]) []
        , errorMessage msgs
        ]
    Validation.Res val ->
      div
        [ class "relative" ]
        [ labeledTextInput label_ attrs [] ]

labeledPasswordInput : LabeledInput msg
labeledPasswordInput label_ attrs _ =
  div
    [ class "validation-input" ]
    [ label [] [ text label_ ]
    , passwordInput attrs []
    ]

labeledPasswordInputValidation : Validation (List String) String -> LabeledInput msg
labeledPasswordInputValidation result label_ attrs _ =
  case result of
    Validation.Err msgs ->
       div
        [ class "relative" ]
        [ labeledPasswordInput label_ (attrs ++ [ class "border-red" ]) []
        , errorMessage msgs
        ]
    Validation.Res val ->
      div
        [ class "relative" ]
        [ labeledPasswordInput label_ attrs [] ]

textArea : List (Attribute msg) -> List (Html msg) -> Html msg
textArea attrs =
  textarea 
    (attrs ++ [ class "textarea not-rounded resize-none" ])

labeledTextArea : Label -> List (Attribute msg) -> List (Html msg) -> Html msg
labeledTextArea label_ attrs _ =
  label
    []
    [ text label_
    , textArea 
      attrs
      []
    ]

calcErrorTop : Int -> String
calcErrorTop n =
  toString <| 
    ( -1 * ( 1.6 + ((toFloat n - 1) * 1.5) ) )

errorMessage : List String -> Html msg
errorMessage msgs =
    div 
      [ class "error-message-wrapper"
      , style [ ( "top", ( calcErrorTop (List.length msgs) ) ++ "rem" ) ]
      ]
      [ ul 
        [ class "error-message p1 rounded list-reset" ]
        (List.map (\msg -> li [] [text msg]) msgs) 
      , div 
        [ class "box-arrow" ]
        []
      ]
