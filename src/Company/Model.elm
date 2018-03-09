module Company.Model exposing (..)

import Adress.Model exposing (Adress, Street, HouseNr, PostCode, Domicile)
import Validation exposing (Validation, (<*>), minLength, maxLength)
type alias CompanyName = String
type alias CompanyId = String
type alias ErrorMsg = String

-- type alias BankAccount =
--   { bankName : String
--   , 
--   }
type alias Company =
  { id : Maybe CompanyId
  , name : CompanyName
  , adress : Adress
  }
empty : Company
empty =
  { id = Nothing
  , name = ""
  , adress = Adress.Model.empty
  }
type alias ValidCompany = Validation (List String) Company
type alias ValidCompanyName = Validation (List String) CompanyName
