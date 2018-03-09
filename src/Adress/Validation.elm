module Adress.Validation exposing (..)

import Adress.Model exposing (Adress, Street, HouseNr, PostCode, Domicile)
import Validation exposing (Validation, (<*>), lengthEquals, isNotEmpty, isInt, maxLength, minLength)

validateStreet : Street -> Validation (List  String) Street
validateStreet val =
  Validation.pure (\a b -> a)
    <*> minLength 4 val
    <*> maxLength 30 val

validateHouseNr : HouseNr -> Validation (List String) HouseNr
validateHouseNr val =
  Validation.pure (\a b -> a)
    <*> isNotEmpty val
    <*> maxLength 12 val

validatePostCode : PostCode -> Validation (List String) PostCode
validatePostCode val =
  Validation.pure (\a b -> a)
    <*> lengthEquals 5 val
    <*> isInt val

validateDomicile : Domicile -> Validation (List String) Domicile
validateDomicile val =
  Validation.pure (\a b -> a)
    <*> minLength 4 val
    <*> maxLength 30 val

validateAdress : Street -> HouseNr -> PostCode -> Domicile -> Validation (List String) Adress
validateAdress street houseNr postCode domicile = 
  Validation.pure Adress
    <*> validateStreet street
    <*> validateHouseNr houseNr
    <*> validatePostCode postCode
    <*> validateDomicile domicile

