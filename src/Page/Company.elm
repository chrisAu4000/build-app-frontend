module Page.Company exposing (..)

import Auth.Model exposing (Token)
import Company.Model exposing (CompanyId)
import Html exposing (Html, div, text)


type alias Model =
    { companyId : CompanyId
    , token : Token
    }


type Msg
    = Msg


init : Token -> CompanyId -> ( Model, Cmd Msg )
init token companyId =
    ( { companyId = companyId
      , token = token
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div
        []
        [ text (toString model) ]
