module Adress.Model exposing (..)

import Validation exposing (Validation)

type alias Street = String
type alias HouseNr = String
type alias PostCode = String
type alias Domicile = String

type alias Adress =
  { street : Street
  , houseNr : HouseNr
  , postCode : PostCode
  , domicile : Domicile
  }

type alias ValidAdress = Validation (List String) Adress

empty : Adress
empty =
  { street = ""
  , houseNr = ""
  , postCode = ""
  , domicile = ""
  }

