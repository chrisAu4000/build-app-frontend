module Company.Validation exposing (..)

import Company.Model exposing (Company, CompanyName, ValidCompanyName, ValidCompany)
import Adress.Model exposing (Adress)
import Adress.Validation exposing (validateAdress)
import Validation exposing ((<*>), minLength, maxLength)

validateCompanyName : CompanyName -> ValidCompanyName
validateCompanyName val =
  Validation.pure (\a b -> a)
    <*> minLength 4 val
    <*> maxLength 30 val

validateCompany : CompanyName -> Adress -> ValidCompany
validateCompany name adress =
  Validation.pure(Company Nothing)
    <*> validateCompanyName name
    <*> validateAdress adress.street adress.houseNr adress.postCode adress.domicile