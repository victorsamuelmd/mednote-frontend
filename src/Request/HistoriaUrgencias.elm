module Request.HistoriaUrgencias exposing (solicitar, guardar)

import Http
import Data.HistoriaUrgencias exposing (..)
import Json.Decode as Decode


server : String
server =
    "http://localhost:8070/historia"


solicitar : (Result Http.Error Model -> msg) -> String -> Cmd msg
solicitar msg authorization =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" authorization
            , Http.header "Content-Type" "application/json"
            ]
        , url = server
        , body = Http.emptyBody
        , expect = Http.expectJson decodeHistoriaUrgencias
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg


guardar : (Result Http.Error String -> msg) -> String -> Model -> Cmd msg
guardar msg authorization historiaUrgencias =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Authorization" authorization
            , Http.header "Content-Type" "application/json"
            ]
        , url = server
        , body = encodeHistoriaUrgencias historiaUrgencias |> Http.jsonBody
        , expect = Http.expectJson Decode.string
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg
