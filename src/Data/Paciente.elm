module Data.Paciente exposing (Output, Paciente, decodePaciente, definirApellidos, definirDocumentoNumero, definirDocumentoTipo, definirFechaNacimiento, definirFechaUltimoIngreso, definirGenero, definirId, definirNombres, definirResidenciaBarrio, definirResidenciaDepartamento, definirResidenciaDireccion, definirResidenciaMunicipio, definirResidenciaPais, definirTelefono, definirUsuarioId, encodeOutput, encodePaciente, initialPaciente)

import Helpers exposing (dateDecoder)
import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as Encode
import Time exposing (Time)


type alias Paciente =
    { id : String
    , usuarioId : String
    , nombres : String
    , apellidos : String
    , genero : String
    , documentoNumero : String
    , documentoTipo : String
    , fechaNacimiento : Time
    , fechaUltimoIngreso : Time
    , telefono : String
    , residenciaPais : String
    , residenciaDepartamento : String
    , residenciaMunicipio : String
    , residenciaBarrio : String
    , residenciaDireccion : String
    }


type alias Output =
    { nombres : String
    , apellidos : String
    , genero : String
    , documentoNumero : String
    , documentoTipo : String
    , fechaNacimiento : Time
    , telefono : String
    , residenciaPais : String
    , residenciaDepartamento : String
    , residenciaMunicipio : String
    , residenciaBarrio : String
    , residenciaDireccion : String
    }


initialPaciente : Paciente
initialPaciente =
    { id = ""
    , usuarioId = ""
    , nombres = ""
    , apellidos = ""
    , genero = ""
    , documentoNumero = ""
    , documentoTipo = ""
    , fechaNacimiento = Time.millisToPosix 0
    , fechaUltimoIngreso = Time.millisToPosix 0
    , telefono = ""
    , residenciaPais = ""
    , residenciaDepartamento = ""
    , residenciaMunicipio = ""
    , residenciaBarrio = ""
    , residenciaDireccion = ""
    }


decodePaciente : Decoder Paciente
decodePaciente =
    decode Paciente
        |> required "id" Decode.string
        |> required "usuarioId" Decode.string
        |> required "nombres" Decode.string
        |> required "apellidos" Decode.string
        |> required "genero" Decode.string
        |> required "documentoNumero" Decode.string
        |> required "documentoTipo" Decode.string
        |> required "fechaNacimiento" dateDecoder
        |> required "fechaUltimoIngreso" dateDecoder
        |> required "telefono" Decode.string
        |> required "residenciaPais" Decode.string
        |> required "residenciaDepartamento" Decode.string
        |> required "residenciaMunicipio" Decode.string
        |> required "residenciaBarrio" Decode.string
        |> required "residenciaDireccion" Decode.string


encodePaciente : Paciente -> Encode.Value
encodePaciente model =
    Encode.object
        [ ( "id", Encode.string model.id )
        , ( "usuarioId", Encode.string model.usuarioId )
        , ( "nombres", Encode.string model.nombres )
        , ( "apellidos", Encode.string model.apellidos )
        , ( "genero", Encode.string model.genero )
        , ( "documentoNumero", Encode.string model.documentoNumero )
        , ( "documentoTipo", Encode.string model.documentoTipo )
        , ( "fechaNacimiento", Encode.string <| Format.formatISO8601 model.fechaNacimiento )
        , ( "telefono", Encode.string model.telefono )
        , ( "residenciaPais", Encode.string model.residenciaPais )
        , ( "residenciaDepartamento", Encode.string model.residenciaDepartamento )
        , ( "residenciaMunicipio", Encode.string model.residenciaMunicipio )
        , ( "residenciaBarrio", Encode.string model.residenciaBarrio )
        , ( "residenciaDireccion", Encode.string model.residenciaDireccion )
        ]


encodeOutput : Output -> Encode.Value
encodeOutput model =
    Encode.object
        [ ( "nombres", Encode.string model.nombres )
        , ( "apellidos", Encode.string model.apellidos )
        , ( "genero", Encode.string model.genero )
        , ( "documentoNumero", Encode.string model.documentoNumero )
        , ( "documentoTipo", Encode.string model.documentoTipo )
        , ( "fechaNacimiento", Encode.string <| Format.formatISO8601 model.fechaNacimiento )
        , ( "telefono", Encode.string model.telefono )
        , ( "residenciaPais", Encode.string model.residenciaPais )
        , ( "residenciaDepartamento", Encode.string model.residenciaDepartamento )
        , ( "residenciaMunicipio", Encode.string model.residenciaMunicipio )
        , ( "residenciaBarrio", Encode.string model.residenciaBarrio )
        , ( "residenciaDireccion", Encode.string model.residenciaDireccion )
        ]


definirId : String -> Paciente -> Paciente
definirId a record =
    { record | id = a }


definirUsuarioId : String -> Paciente -> Paciente
definirUsuarioId a record =
    { record | usuarioId = a }


definirNombres : String -> Paciente -> Paciente
definirNombres a record =
    { record | nombres = a }


definirApellidos : String -> Paciente -> Paciente
definirApellidos a record =
    { record | apellidos = a }


definirGenero : String -> Paciente -> Paciente
definirGenero a record =
    { record | genero = a }


definirDocumentoNumero : String -> Paciente -> Paciente
definirDocumentoNumero a record =
    { record | documentoNumero = a }


definirDocumentoTipo : String -> Paciente -> Paciente
definirDocumentoTipo a record =
    { record | documentoTipo = a }


definirFechaNacimiento : String -> Paciente -> Paciente
definirFechaNacimiento a record =
    case Time.Posix a of
        Ok date ->
            { record | fechaNacimiento = date }

        Err _ ->
            record


definirFechaUltimoIngreso : String -> Paciente -> Paciente
definirFechaUltimoIngreso a record =
    case Date.fromString a of
        Ok date ->
            { record | fechaUltimoIngreso = date }

        Err _ ->
            record


definirTelefono : String -> Paciente -> Paciente
definirTelefono a record =
    { record | telefono = a }


definirResidenciaPais : String -> Paciente -> Paciente
definirResidenciaPais a record =
    { record | residenciaPais = a }


definirResidenciaDepartamento : String -> Paciente -> Paciente
definirResidenciaDepartamento a record =
    { record | residenciaDepartamento = a }


definirResidenciaMunicipio : String -> Paciente -> Paciente
definirResidenciaMunicipio a record =
    { record | residenciaMunicipio = a }


definirResidenciaBarrio : String -> Paciente -> Paciente
definirResidenciaBarrio a record =
    { record | residenciaBarrio = a }


definirResidenciaDireccion : String -> Paciente -> Paciente
definirResidenciaDireccion a record =
    { record | residenciaDireccion = a }
