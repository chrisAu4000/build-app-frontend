module User.Validation exposing (..)

import User.Model exposing (Email, Username, Password)
import Validation exposing (Validation, (<*>), minLength, maxLength, isEmail, includesIntegral, equals)

validateUsername : String -> Validation (List String) String
validateUsername username =
  Validation.pure (\a b -> a)
    <*> minLength 4 username
    <*> maxLength 20 username

validateEmail : String -> Validation (List String) String
validateEmail email =
  Validation.pure (\a b c -> a)
    <*> minLength 6 email
    <*> maxLength 20 email
    <*> isEmail email

validatePassword : Password -> Validation (List String) String
validatePassword password = 
  Validation.pure (\a b c -> a)
    <*> minLength 8 password
    <*> maxLength 50 password
    <*> includesIntegral password

validateVerification : String -> Password -> Validation (List String) String
validateVerification = equals
