module Position.Model exposing 
  ( FormState
  , FormModel(..)
  , Position
  , PositionId
  )

type alias PositionId = String
type alias Errors = List String

type alias FormState =
  { errors : Errors
  , description : String
  , unit : String
  }

type FormModel
  = NotAsked FormState
  | Loading FormState
  | Success FormState
  | Failure FormState

type alias Position =
  { id : PositionId
  , description : String
  , unit : String
  }