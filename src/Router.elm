module Router exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (oneOf, map, top, parseHash, s, (</>), string, Parser)


type Route
    = AdminRoute
    | MedicoRoute String
    | LoginRoute
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map LoginRoute top
        , map AdminRoute (s "admin")
        , map MedicoRoute (s "medico" </> string)
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
