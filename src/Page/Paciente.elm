module Page.Pacientes exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Data.HistoriaUrgencias exposing (..)


inputControl : String -> String -> String -> (String -> msg) -> Html msg
inputControl labelText nameText valueText msg =
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , input
            [ type_ "text"
            , class "form-control"
            , name nameText
            , id nameText
            , defaultValue valueText
            , onInput msg
            ]
            []
        ]


textareaControl : String -> String -> String -> (String -> msg) -> Html msg
textareaControl labelText nameText valueText msg =
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , textarea
            [ class "form-control"
            , name nameText
            , id nameText
            , defaultValue valueText
            , onInput msg
            ]
            []
        ]


view msg =
    div []
        [ inputControl
            "Nombres"
            "nombres"
            ""
            DefinirNombres
        , inputControl
            "Apellidos"
            "apellidos"
            ""
            DefinirApellidos
        , inputControl
            "Genero"
            "genero"
            ""
            DefinirGenero
        , inputControl
            "Documento Numero"
            "documentoNumero"
            ""
            DefinirDocumentoNumero
        , inputControl
            "Documento Tipo"
            "documentoTipo"
            ""
            DefinirDocumentoTipo
        , inputControl
            "Fecha Nacimiento"
            "fechaNacimiento"
            ""
            DefinirFechaNacimiento
        , inputControl
            "Telefono"
            "telefono"
            ""
            DefinirTelefono
        , inputControl
            "Residencia Pais"
            "residenciaPais"
            ""
            DefinirResidenciaPais
        , inputControl
            "Residencia Departamento"
            "residenciaDepartamento"
            ""
            DefinirResidenciaDepartamento
        , inputControl
            "Residencia Municipio"
            "residenciaMunicipio"
            ""
            DefinirResidenciaMunicipio
        , inputControl
            "Residencia Barrio"
            "residenciaBarrio"
            ""
            DefinirResidenciaBarrio
        , inputControl
            "Residencia Direccion"
            "residenciaDireccion"
            ""
            DefinirResidenciaDireccion
        ]
