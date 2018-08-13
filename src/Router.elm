module Router exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (oneOf, map, top, parseHash, s, (</>), string, Parser)


type Route
    = AdminRoute
    | MedicoRoute String
    | PacientesRoute
    | LoginRoute
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map LoginRoute top
        , map AdminRoute (s "admin")
        , map MedicoRoute (s "medico" </> string)
        , map PacientesRoute (s "pacientes")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
