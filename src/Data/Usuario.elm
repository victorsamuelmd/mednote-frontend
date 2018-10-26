module Data.Usuario exposing
    ( Usuario
    , UsuarioCrear
    , crearUsuarioEncoder
    , decodeUsuario
    , definirCorreoElectronico
    , definirGrupo
    , definirPalabraClave
    , definirUsuario
    , encodeUsuarioEditar
    )

import Json.Decode as JsDecode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Time


type alias Usuario =
    { id : String
    , usuario : String
    , correoElectronico : String
    , grupo : String
    , fechaCreacion : Int
    }


type alias UsuarioCrear =
    { usuario : String
    , correoElectronico : String
    , grupo : String
    , palabraClave : String
    }


definirUsuario : String -> UsuarioCrear -> UsuarioCrear
definirUsuario str usr =
    { usr | usuario = str }


definirCorreoElectronico : String -> UsuarioCrear -> UsuarioCrear
definirCorreoElectronico str usr =
    { usr | correoElectronico = str }


definirGrupo : String -> UsuarioCrear -> UsuarioCrear
definirGrupo str usr =
    { usr | grupo = str }


definirPalabraClave : String -> UsuarioCrear -> UsuarioCrear
definirPalabraClave str usr =
    { usr | palabraClave = str }


crearUsuarioEncoder : UsuarioCrear -> Encode.Value
crearUsuarioEncoder usr =
    Encode.object
        [ ( "usuario", Encode.string usr.usuario )
        , ( "grupo", Encode.string usr.grupo )
        , ( "palabraClave", Encode.string usr.palabraClave )
        , ( "correoElectronico", Encode.string usr.correoElectronico )
        ]


decodeUsuario : JsDecode.Decoder Usuario
decodeUsuario =
    JsDecode.succeed Usuario
        |> Decode.required "id" JsDecode.string
        |> Decode.required "usuario" JsDecode.string
        |> Decode.required "correoElectronico" JsDecode.string
        |> Decode.required "grupo" JsDecode.string
        |> Decode.required "fechaCreacion" JsDecode.int


encodeUsuarioEditar usr =
    Encode.object
        [ ( "grupo", Encode.string usr.grupo )
        , ( "correoElectronico", Encode.string usr.correoElectronico )
        ]
