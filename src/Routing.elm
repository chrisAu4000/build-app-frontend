module Routing exposing (Route(..), matchers)

import UrlParser exposing (..)

type Route
  = NotFound
  | Login
  | Registration
  | Home
  | Company
  | EditCompany String
  | AddPosition

matchers : Parser (Route -> a) a
matchers =
  oneOf
    [ UrlParser.map NotFound top
    , UrlParser.map Login (s "login")
    , UrlParser.map Registration (s "registration")
    , UrlParser.map Home (s "home")
    , UrlParser.map Company (s "company")
    , UrlParser.map EditCompany (s ("companyEdit") </> string)
    , UrlParser.map AddPosition (s "position")
    ]
-- matchers : Parser (Route -> a) a
-- matchers =
--   oneOf
--     [ map AddPosition (s "position")
--     , map EditPosition (s "position" </> string)
--     ]

-- parseRoute : Route -> String
-- parseRoute route =
--   let pieces =
--     case route of
--       AddPosition -> [ "position"]
--       EditPosition positionId -> [ "position", positionId ]
--   in
--     "#/" ++ String.join "/" pieces

-- parseLocation : Location -> Maybe Route
-- parseLocation = parseHash matchers