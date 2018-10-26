module Router exposing (Route(..), matchers, parseLocation)

import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


type Route
    = AdminRoute
    | MedicoRoute
    | PacientesRoute
    | EditarPacienteRoute String
    | UrgenciasRoute String
    | LoginRoute
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map LoginRoute top
        , map AdminRoute (s "admin")
        , map MedicoRoute (s "medico")
        , map PacientesRoute (s "pacientes")
        , map EditarPacienteRoute (s "editar" </> string)
        , map UrgenciasRoute (s "urgencias" </> string)
        ]


parseLocation : Url.Url -> Route
parseLocation url =
    case parse matchers url of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
