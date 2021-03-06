module Data.Session exposing (Session, decodeSession)

import Json.Decode as Decode
    exposing
        ( Decoder
        , andThen
        , fail
        , field
        , string
        , succeed
        )


type Grupo
    = Administrador
    | Medico
    | Facturador
    | Enfermera


type alias Session =
    { usuario : String
    , grupo : Grupo
    , autorizacion : String
    , id : String
    }


grupoDecoder : Decoder Grupo
grupoDecoder =
    string
        |> andThen
            (\string ->
                case string of
                    "Administrador" ->
                        succeed Administrador

                    "Medico" ->
                        succeed Medico

                    "Facturador" ->
                        succeed Facturador

                    "Enfermera" ->
                        succeed Enfermera

                    _ ->
                        fail "Grupo Invalido"
            )


decodeSession : Decoder Session
decodeSession =
    Decode.map4 Session
        (field "usuario" Decode.string)
        (field "grupo" grupoDecoder)
        (field "autorizacion" Decode.string)
        (field "id" Decode.string)
