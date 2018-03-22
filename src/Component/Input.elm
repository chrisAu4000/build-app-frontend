module Component.Input exposing (..)

import Data.ValidationInput as VInput exposing (ValidationInput)
import Html exposing (Attribute, Html, div, input, label, li, form, text, textarea, ul)
import Html.Attributes exposing (autocomplete, class, id, for, type_, required, style, value)
import Html.Events exposing (onInput)
type alias Input msg = List (Attribute msg) -> Html msg
type alias LabeledInput msg = String -> List (Attribute msg) -> Html msg


type alias Id = String

textInput : Input msg
textInput attrs =
  input 
    ( [ type_ "text", class "input not-rounded", autocomplete True] ++ attrs )
    []

passwordInput : Input msg
passwordInput attrs =
  input 
    ( [ type_ "password", class "input not-rounded", autocomplete False] ++ attrs )
    []

fileInput : Input msg
fileInput attrs =
  input
    ( [ type_ "file", class "input not-rounded" ] ++ attrs)
    []

textArea : List (Attribute msg) -> List (Html msg) -> Html msg
textArea attrs =
  textarea 
    (attrs ++ [ class "textarea not-rounded resize-none" ])

labeledTextInput : LabeledInput msg
labeledTextInput label_ attrs =
  div
    [ class "validation-input" ]
    [ label [] [ text label_ ]
    , textInput attrs
    ]

labeledPasswordInput : LabeledInput msg
labeledPasswordInput label_ attrs =
  div
    [ class "validation-input" ]
    [ label [] [ text label_ ]
    , passwordInput attrs
    ]

labeledFileInput : Id -> LabeledInput msg
labeledFileInput id_ label_ attrs =
  div
    [ class "btn bg-blue white text-middle"]
    [ fileInput (attrs ++ [ id id_ ])
    , label [ for id_ ] [ text label_ ]
    ]

labeledTextArea : String -> List (Attribute msg) -> List (Html msg) -> Html msg
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

invalidInput : Html msg -> List String -> Html msg
invalidInput input_ errors =
  div
    [ class "relative" ]
    [ input_
    , errorMessage errors
    ]

labeledTextInputValidation : (String -> msg) -> String -> ValidationInput String -> Html msg
labeledTextInputValidation msg label_ validation =
  case validation of
    VInput.Err msgs val ->
      invalidInput
        (labeledTextInput label_ 
          [ class "border-red"
          , value val
          , onInput msg 
          ]
        )
        msgs
    VInput.Ok val ->
      div
        [ class "relative" ]
        [ labeledTextInput label_ [ value val, onInput msg ] ]

labeledPasswordInputValidation : (String -> msg) -> String -> ValidationInput String-> Html msg
labeledPasswordInputValidation msg label_ result =
  case result of
    VInput.Err errors val ->
       invalidInput
        (labeledPasswordInput label_
          [ class "border-red"
          , value val
          , onInput msg 
          ]
        )
        errors
    VInput.Ok val ->
      div
        [ class "relative" ]
        [ labeledPasswordInput label_ [ value val, onInput msg ] ]
