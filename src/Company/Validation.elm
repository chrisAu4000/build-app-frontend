module Company.Validation exposing (..)

import Adress.Validation exposing (validateAdress)
import Company.Model exposing (Company, CompanyName, Logo)
import Data.ValidationInput as ValidationInput exposing ((<*>), ValidationInput, maxLength, minLength, pure)


validateCompanyName : CompanyName -> CompanyName
validateCompanyName val =
    pure (\a b -> a)
        <*> minLength 4 val
        <*> maxLength 30 val


validateCompanyLogo : Logo -> ValidationInput Logo
validateCompanyLogo =
    pure


validateCompany : Company -> Company
validateCompany company =
    Company Nothing
        (validateCompanyName company.name)
        company.logo
        (validateAdress company.adress)
