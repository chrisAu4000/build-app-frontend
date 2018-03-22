module Adress.Validation exposing (..)

import Adress.Model exposing (Adress, Street, HouseNr, PostCode, Domicile)
import Data.Validation exposing (Validation, (<*>), lengthEquals, pure, isNotEmpty, isInt, maxLength, minLength)

validateStreet : Street -> Validation (List  String) Street
validateStreet val =
  pure (\a b -> a)
    <*> minLength 4 val
    <*> maxLength 30 val

validateHouseNr : HouseNr -> Validation (List String) HouseNr
validateHouseNr val =
  pure (\a b -> a)
    <*> isNotEmpty val
    <*> maxLength 12 val

validatePostCode : PostCode -> Validation (List String) PostCode
validatePostCode val =
  pure (\a b -> a)
    <*> lengthEquals 5 val
    <*> isInt val

validateDomicile : Domicile -> Validation (List String) Domicile
validateDomicile val =
  pure (\a b -> a)
    <*> minLength 4 val
    <*> maxLength 30 val

validateAdress : Street -> HouseNr -> PostCode -> Domicile -> Validation (List String) Adress
validateAdress street houseNr postCode domicile = 
  pure Adress
    <*> validateStreet street
    <*> validateHouseNr houseNr
    <*> validatePostCode postCode
    <*> validateDomicile domicile

