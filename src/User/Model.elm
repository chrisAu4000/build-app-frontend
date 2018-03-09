module User.Model exposing (AuthResponse, User, Token, Username, Email, Password)

-- import Company.Model exposing (Company)

type alias Username = String
type alias Password = String
type alias Email = String
type alias Token = String

type alias AuthResponse =
  { jwt : Token
  , user : User
  }

type alias User =
  { id : String
  , email : Email
  , provider : String
  , username : Username
  }