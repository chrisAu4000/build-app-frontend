module Adress.Validation exposing (..)

-- import Data.Validation exposing (Validation, (<*>), lengthEquals, pure, isNotEmpty, isInt, maxLength, minLength)

import Adress.Model exposing (Adress, Domicile, HouseNr, PostCode, Street)
import Data.ValidationInput as ValidationInput exposing ((<*>), ValidationInput, isInt, lengthEquals, maxLength, minLength, pure)


validateStreet : Street -> Street
validateStreet val =
    pure (\a b -> a)
        <*> minLength 4 val
        <*> maxLength 30 val


validateHouseNr : HouseNr -> HouseNr
validateHouseNr val =
    pure (\a b -> a)
        <*> minLength 1 val
        <*> maxLength 12 val


validatePostCode : PostCode -> PostCode
validatePostCode val =
    pure (\a b -> a)
        <*> lengthEquals 5 val
        <*> isInt val


validateDomicile : Domicile -> Domicile
validateDomicile val =
    pure (\a b -> a)
        <*> minLength 4 val
        <*> maxLength 30 val


validateAdress : Adress -> Adress
validateAdress adress =
    Adress
        (validateStreet adress.street)
        (validateHouseNr adress.houseNr)
        (validatePostCode adress.postCode)
        (validateDomicile adress.domicile)
