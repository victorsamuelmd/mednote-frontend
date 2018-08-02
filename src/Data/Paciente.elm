module Data.Paciente exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode exposing (field, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type alias Paciente =
    { id : String
    , usuarioId : String
    , nombres : String
    , apellidos : String
    , genero : String
    , documentoNumero : String
    , documentoTipo : String
    , fechaNacimiento : String
    , fechaUltimoIngreso : String
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
    , fechaNacimiento = ""
    , fechaUltimoIngreso = ""
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
        |> required "fechaNacimiento" Decode.string
        |> required "fechaUltimoIngreso" Decode.string
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
        , ( "fechaNacimiento", Encode.string model.fechaNacimiento )
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
    { record | fechaNacimiento = a }


definirFechaUltimoIngreso : String -> Paciente -> Paciente
definirFechaUltimoIngreso a record =
    { record | fechaUltimoIngreso = a }


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
