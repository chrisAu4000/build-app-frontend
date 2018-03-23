module User.Validation exposing (..)

import Data.ValidationInput as ValidationInput
    exposing
        ( (<*>)
        , ValidationInput
        , equals
        , includesIntegral
        , isEmail
        , maxLength
        , minLength
        )
import User.Model exposing (Email, Password, Username)


-- import Validation exposing (Validation, minLength, maxLength, isEmail, includesIntegral, equals)


validateUsername : ValidationInput String -> ValidationInput String
validateUsername username =
    ValidationInput.pure (\a b -> a)
        <*> minLength 4 username
        <*> maxLength 20 username


validateEmail : ValidationInput String -> ValidationInput String
validateEmail =
    isEmail


validatePassword : ValidationInput Password -> ValidationInput String
validatePassword password =
    ValidationInput.pure (\a b c -> a)
        <*> minLength 8 password
        <*> maxLength 50 password
        <*> includesIntegral password


validateVerification : ValidationInput String -> ValidationInput Password -> ValidationInput String
validateVerification =
    equals
