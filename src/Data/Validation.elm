module Data.Validation exposing (..)

import Regex exposing (regex)

type Validation e a
  = Err e
  | Res a

map : (a -> b) -> Validation e a -> Validation e b
map f validation =
  case validation of
    Err e -> Err e
    Res a -> Res (f a)

mapErr : (e -> e2) -> Validation e a -> Validation e2 a
mapErr f validation =
  case validation of
    Err e -> Err (f e)
    Res a -> Res a

ap : Validation appendable (a -> b) -> Validation appendable a -> Validation appendable b
ap validation1 validation2 =
  case validation2 of
    Err e1 -> 
      case validation1 of
        Err e2 -> Err (e2 ++ e1)
        Res _ -> Err e1
    Res a -> map (\f -> f a) validation1

(<*>) : Validation appendable (a -> b) -> Validation appendable a -> Validation appendable b
(<*>) = ap

pure : a -> Validation e a
pure val = Res val

lift1 : (a -> b) -> Validation e a -> Validation e b
lift1 f a = map f a

lift2 : (a -> b -> c) ->
  Validation appendable a ->
  Validation appendable b ->
  Validation appendable c
lift2 f a b = ap (map f a) b

toBool : Validation e a -> Bool
toBool validation =
  case validation of
    Err _ -> False
    Res _ -> True

toResult : Validation e a -> Result e a
toResult validation =
  case validation of
    Err e -> Result.Err e
    Res a -> Result.Ok a

toMaybe : Validation e a -> Maybe a
toMaybe validation =
  case validation of
    Err _ -> Nothing
    Res a -> Just a

fromMaybe : Maybe a -> String -> Validation (List String) a
fromMaybe mb err =
  case mb of
    Nothing -> Err [ err ]
    Just val -> Res val

isNotEmpty : String -> Validation (List String) String
isNotEmpty val =
  if val == ""
  then Err ["This field is required."]
  else Res val

includesString : String -> String -> Validation (List String) String
includesString str val =
  if String.contains str val
  then Res val
  else Err ["This input must contain '" ++ str ++ "'." ]

isEmail : String -> Validation (List String) String
isEmail val =
  if String.contains "@" val
  then Res val
  else Err ["Please Enter a valid email address."]

minLength : Int -> String -> Validation (List String) String
minLength n val =
  if (String.length val) < n
  then Err ["This input must have a minimum length of " ++ (toString n) ++ "."]
  else Res val

maxLength : Int -> String -> Validation (List String) String
maxLength n val =
  if (String.length val) > n
  then Err ["This input must have a maximum length of " ++ (toString n) ++ "."]
  else Res val

includesIntegral : String -> Validation (List String) String
includesIntegral val =
  if Regex.contains (regex "[0-9]") val
  then Res val
  else Err ["This input must contain at least one number."]

includesSymbole : String -> Validation (List String) String
includesSymbole val =
  if Regex.contains (regex "*@!#%&()^~{}[]") val
  then Res val
  else Err ["This input must contain at least one number."]

equals : a -> a -> Validation (List String) a
equals a b =
  if a == b
  then Res a
  else Err ["Password and Verification must be equal."]

lengthEquals : Int -> String -> Validation (List String) String
lengthEquals n s =
  if (String.length s) == n
  then Res s
  else Err ["This input must have a length of " ++ (toString n) ++ "."]

isInt : String -> Validation (List String) String
isInt a =
  case String.toInt a of
    Result.Err _ -> Err ["Only digits are allowed here."]
    Result.Ok val -> Res a