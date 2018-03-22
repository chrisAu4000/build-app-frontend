module User.Validation exposing (..)

import User.Model exposing (Email, Username, Password)
import Data.ValidationInput as ValidationInput exposing
  ( ValidationInput
  , (<*>)
  , minLength
  , maxLength
  , isEmail
  , includesIntegral
  , equals
  )
-- import Validation exposing (Validation, minLength, maxLength, isEmail, includesIntegral, equals)

validateUsername : String -> ValidationInput String
validateUsername username =
  ValidationInput.pure (\a b -> a)
    <*> minLength 4 username
    <*> maxLength 20 username

validateEmail : String -> ValidationInput String
validateEmail = isEmail

validatePassword : Password -> ValidationInput String
validatePassword password = 
  ValidationInput.pure (\a b c -> a)
    <*> minLength 8 password
    <*> maxLength 50 password
    <*> includesIntegral password

validateVerification : String -> Password -> ValidationInput String
validateVerification = equals
