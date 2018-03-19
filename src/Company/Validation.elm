module Company.Validation exposing (..)

import Adress.Model exposing (Adress)
import Adress.Validation exposing (validateAdress)
import Either exposing (Either)
import Company.Model exposing (Company, CompanyName, ValidCompanyName, ValidCompany)
import FileReader exposing (NativeFile)
import Validation exposing (Validation, (<*>), minLength, maxLength)

type alias Logo = Maybe (Either NativeFile String)

validateCompanyName : CompanyName -> ValidCompanyName
validateCompanyName val =
  Validation.pure (\a b -> a)
    <*> minLength 4 val
    <*> maxLength 30 val

validateCompanyLogo : Logo -> Validation (List String) Logo
validateCompanyLogo logo =
  case logo of
    Nothing -> Validation.Res Nothing
    Just logo -> Validation.Res (Just logo)

validateCompany : CompanyName -> Logo -> Adress -> ValidCompany
validateCompany name logo adress =
  Validation.pure(Company Nothing)
    <*> validateCompanyName name
    <*> validateCompanyLogo logo
    <*> validateAdress adress.street adress.houseNr adress.postCode adress.domicile