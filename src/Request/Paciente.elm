module Request.Paciente exposing (crear, obtener, obtenerLista, editar)

import Http as Http
import HttpBuilder
import Json.Decode as Decode
import Data.Paciente as Paciente
import Request.ServerHelper exposing (serverUrl)
import Data.Session exposing (Session)


crear : Session -> Paciente.Output -> (Result Http.Error String -> msg) -> Cmd msg
crear { autorizacion } output msg =
    HttpBuilder.post (serverUrl ++ "/perfiles")
        |> HttpBuilder.withJsonBody (Paciente.encodeOutput output)
        |> withAuthorization autorizacion
        |> HttpBuilder.withExpectString
        |> HttpBuilder.send msg


obtenerLista :
    Session
    -> { a | porNombres : String, porApellidos : String, porDocumentoNumero : String }
    -> (Result Http.Error (List Paciente.Paciente) -> msg)
    -> Cmd msg
obtenerLista { autorizacion } { porNombres, porApellidos, porDocumentoNumero } msg =
    HttpBuilder.get (serverUrl ++ "/perfiles")
        |> HttpBuilder.withBody Http.emptyBody
        |> HttpBuilder.withExpectJson (Decode.list Paciente.decodePaciente)
        |> HttpBuilder.withQueryParams
            [ ( "nombres", porNombres )
            , ( "apellidos", porApellidos )
            , ( "documentoNumero", porDocumentoNumero )
            ]
        |> withAuthorization autorizacion
        |> HttpBuilder.send msg


obtener : Session -> String -> (Result Http.Error Paciente.Paciente -> msg) -> Cmd msg
obtener { autorizacion } id msg =
    HttpBuilder.get (serverUrl ++ "/perfiles/" ++ id)
        |> HttpBuilder.withBody Http.emptyBody
        |> HttpBuilder.withExpectJson Paciente.decodePaciente
        |> withAuthorization autorizacion
        |> HttpBuilder.send msg


editar : Session -> String -> Paciente.Output -> (Result Http.Error String -> msg) -> Cmd msg
editar { autorizacion } id output msg =
    HttpBuilder.put (serverUrl ++ "/perfiles/" ++ id)
        |> HttpBuilder.withJsonBody (Paciente.encodeOutput output)
        |> HttpBuilder.withExpectString
        |> withAuthorization autorizacion
        |> HttpBuilder.send msg



-- Internals


withAuthorization :
    String
    -> HttpBuilder.RequestBuilder a
    -> HttpBuilder.RequestBuilder a
withAuthorization a =
    HttpBuilder.withHeader "Authorization" a
