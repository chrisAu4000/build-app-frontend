module Page.NotFound exposing (..)

import Html exposing (Html, text)


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


view : Model -> Html msg
view model =
    text "Not Found"
