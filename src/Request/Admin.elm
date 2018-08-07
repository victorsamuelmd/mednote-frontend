module Request.Admin exposing (..)

import Http
import Json.Decode as Decode
import Data.Usuario exposing (..)


solicitarUsuarios :
    String
    -> (Result Http.Error (List Usuario) -> msg)
    -> Cmd msg
solicitarUsuarios str msg =
    let
        server =
            "http://localhost:8070/usuarios"
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


crearUsuario :
    { a | authorization : String, usuarioCrear : UsuarioCrear }
    -> (Result Http.Error String -> msg)
    -> Cmd msg
crearUsuario model msg =
    let
        server =
            "http://localhost:8070/usuarios"
    in
        Http.send msg <|
            Http.request
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" model.authorization
                    , Http.header "Content-Type" "application/json"
                    ]
                , url = server
                , body = crearUsuarioEncoder model.usuarioCrear |> Http.jsonBody
                , expect = Http.expectJson Decode.string
                , timeout = Nothing
                , withCredentials = False
                }
