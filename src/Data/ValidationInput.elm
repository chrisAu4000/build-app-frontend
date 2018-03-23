module Data.ValidationInput exposing (..)

import Data.Validation as Validation exposing (Validation)


type ValidationInput a
    = Err (List String) a
    | Ok a


map : (a -> b) -> ValidationInput a -> ValidationInput b
map f val =
    case val of
        Ok a ->
            Ok (f a)

        Err errs a ->
            Err errs (f a)


mapErr : (List String -> List String) -> ValidationInput a -> ValidationInput a
mapErr f val =
    case val of
        Ok _ ->
            val

        Err errs a ->
            Err (f errs) a


ap : ValidationInput (a -> b) -> ValidationInput a -> ValidationInput b
ap v1 v2 =
    case v2 of
        Ok a ->
            map (\f -> f a) v1

        Err errs2 a ->
            case v1 of
                Err errs1 f ->
                    Err (errs1 ++ errs2) (f a)

                Ok f ->
                    map f v2


(<*>) : ValidationInput (a -> b) -> ValidationInput a -> ValidationInput b
(<*>) =
    ap


pure : a -> ValidationInput a
pure a =
    Ok a


get : ValidationInput a -> a
get val =
    case val of
        Ok a ->
            a

        Err _ a ->
            a


fromValidation : a -> Validation (List String) a -> ValidationInput a
fromValidation val validation =
    case validation of
        Validation.Err errs ->
            Err errs val

        Validation.Res val_ ->
            Ok val_


minLength : Int -> ValidationInput String -> ValidationInput String
minLength n val =
    let
        value =
            get val
    in
    value
        |> Validation.minLength n
        |> fromValidation value


maxLength : Int -> ValidationInput String -> ValidationInput String
maxLength n val =
    let
        value =
            get val
    in
    (fromValidation value << Validation.maxLength n) value


isEmail : ValidationInput String -> ValidationInput String
isEmail val =
    pure (\a b c d -> a)
        <*> minLength 3 val
        <*> maxLength 50 val
        <*> includesString "@" val
        <*> includesString "." val
        |> mapErr (always [ "Please enter a valid E-Mail adress." ])


isInt : ValidationInput String -> ValidationInput String
isInt val =
    let
        value =
            get val
    in
    (fromValidation value << Validation.isInt) value


lengthEquals : Int -> ValidationInput String -> ValidationInput String
lengthEquals n val =
    let
        value =
            get val
    in
    (fromValidation value << Validation.lengthEquals n) value


includesString : String -> ValidationInput String -> ValidationInput String
includesString str val =
    let
        value =
            get val
    in
    (fromValidation value << Validation.includesString str) value


includesIntegral : ValidationInput String -> ValidationInput String
includesIntegral val =
    let
        value =
            get val
    in
    (fromValidation value << Validation.includesIntegral) value


equals : ValidationInput a -> ValidationInput a -> ValidationInput a
equals a b =
    let
        valueA =
            get a

        valueB =
            get b
    in
    Validation.equals valueA valueB
        |> fromValidation valueA
