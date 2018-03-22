module Data.ValidationInput exposing (..)

import Regex exposing (Regex, regex)

type ValidationInput a
  = Err (List String) a 
  | Ok a

map : (a -> b) -> ValidationInput a -> ValidationInput b
map f val = 
  case val of
    Ok a -> Ok (f a)
    Err errs a -> Err errs (f a)

mapErr : (List String -> List String) -> ValidationInput a -> ValidationInput a
mapErr f val =
  case val of
    Ok _ -> val
    Err errs a -> Err (f errs) a

ap : ValidationInput (a -> b) -> ValidationInput a -> ValidationInput b
ap v1 v2 =
  case v2 of
    Ok a -> map (\f -> f a) v1
    Err errs2 a ->
      case v1 of
        Err errs1 f -> Err (errs1 ++ errs2) (f a)
        Ok f -> map f v2

(<*>) : ValidationInput (a -> b) -> ValidationInput a -> ValidationInput b
(<*>) = ap

pure : a -> ValidationInput a
pure a = Ok a

get : ValidationInput a -> a
get val =
  case val of
    Ok a -> a
    Err _ a -> a

minLength : Int -> String -> ValidationInput String
minLength n val =
  if (String.length val) < n
  then Err ["This input must have a minimum length of " ++ (toString n) ++ "."] val
  else Ok val

maxLength : Int -> String -> ValidationInput String
maxLength n val =
  if (String.length val) >= n
  then Err ["This input must have a maximum length of " ++ (toString n) ++ "."] val
  else Ok val

isEmail : String -> ValidationInput String
isEmail val =
  pure (\a b c d -> a)
  <*> minLength 3 val
  <*> maxLength 50 val
  <*> includesString "@" val
  <*> includesString "." val
  |> mapErr (always ["Please enter a valid E-Mail adress."])

includesString : String -> String -> ValidationInput String
includesString str val =
  if String.contains str val
  then Ok val
  else Err ["This input must contain '" ++ str ++ "'." ] val

includesIntegral : String -> ValidationInput String
includesIntegral val =
  if Regex.contains (regex "[0-9]") val
  then Ok val
  else Err ["This input must contain at least one number."] val

equals : a -> a -> ValidationInput a
equals a b =
  if a == b
  then Ok b
  else Err ["Password and Verification must be equal."] a