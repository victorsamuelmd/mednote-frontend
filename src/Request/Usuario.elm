module Request.Usuario exposing (..)

import Http
import HttpBuilder
import Json.Decode as Decode
import Json.Encode as Encode
import Data.Usuario exposing (..)
import Request.ServerHelper exposing (serverUrl)


solicitar :
    String
    -> (Result Http.Error (List Usuario) -> msg)
    -> Cmd msg
solicitar str msg =
    let
        server =
            serverUrl ++ "/usuarios"
    in
        Http.send msg <|
            Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Authorization" str
                    , Http.header "Content-Type" "application/json"
                    ]
                , url = server
                , body = Http.emptyBody
                , expect = Http.expectJson (Decode.list decodeUsuario)
                , timeout = Nothing
                , withCredentials = False
                }


crear :
    { a | autorizacion : String, usuarioCrear : UsuarioCrear }
    -> (Result Http.Error String -> msg)
    -> Cmd msg
crear { autorizacion, usuarioCrear } msg =
    HttpBuilder.post (serverUrl ++ "/usuarios")
        |> HttpBuilder.withJsonBody (crearUsuarioEncoder usuarioCrear)
        |> HttpBuilder.withExpectString
        |> HttpBuilder.withHeader "Authorization" autorizacion
        |> HttpBuilder.withHeader "Content-Type" "application/json"
        |> HttpBuilder.send msg


editar :
    { a | autorizacion : String, usuario : Usuario }
    -> (Result Http.Error String -> msg)
    -> Cmd msg
editar { autorizacion, usuario } msg =
    HttpBuilder.put (serverUrl ++ "/usuarios/" ++ usuario.id)
        |> HttpBuilder.withJsonBody (encodeUsuarioEditar usuario)
        |> HttpBuilder.withExpect (Http.expectString)
        |> HttpBuilder.withHeader "Authorization" autorizacion
        |> HttpBuilder.send msg
