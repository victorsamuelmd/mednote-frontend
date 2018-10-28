module Page.PacienteForm exposing (Values, form, prellenar)

import Char
import Data.Paciente exposing (Output, Paciente)
import Form exposing (Form)
import Form.Value as Value exposing (Value)
import Time


type alias Values =
    { nombres : Value String
    , apellidos : Value String
    , genero : Value String
    , documentoNumero : Value String
    , documentoTipo : Value String
    , fechaNacimiento : Value String
    , telefono : Value String
    , residenciaPais : Value String
    , residenciaDepartamento : Value String
    , residenciaMunicipio : Value String
    , residenciaBarrio : Value String
    , residenciaDireccion : Value String
    }


form : Form Values Output
form =
    let
        nombresField =
            Form.textField
                { parser = Ok
                , value = .nombres
                , update = \value values -> { values | nombres = value }
                , attributes = { label = "Nombres", placeholder = "Nombres" }
                }

        apellidosField =
            Form.textField
                { parser =
                    \apellidos ->
                        if String.length apellidos < 2 || String.any Char.isDigit apellidos then
                            Result.Err "Debe contener al menos 2 caracteres y solo letras"

                        else
                            Result.Ok apellidos
                , value = .apellidos
                , update = \value values -> { values | apellidos = value }
                , attributes = { label = "Apellidos", placeholder = "Apellidos" }
                }

        generoField =
            Form.radioField
                { parser = Ok
                , value = .genero
                , update = \value values -> { values | genero = value }
                , attributes =
                    { label = "Genero"
                    , options =
                        [ ( "F", "Femenino" )
                        , ( "M", "Masculino" )
                        , ( "I", "Indeterminado" )
                        ]
                    }
                }

        documentoNumeroField =
            Form.textField
                { parser = Ok
                , value = .documentoNumero
                , update = \value values -> { values | documentoNumero = value }
                , attributes = { label = "Numero de Documento", placeholder = "Documento Numero" }
                }

        documentoTipoField =
            Form.selectField
                { parser = Ok
                , value = .documentoTipo
                , update = \value values -> { values | documentoTipo = value }
                , attributes =
                    { label = "Tipo Documento"
                    , placeholder = "Tipo Documento"
                    , options =
                        [ ( "CC", "Cedula de ciudadania" )
                        , ( "CE", "Cedula de extranjeria" )
                        , ( "TI", "Tarjeta de Identidad" )
                        , ( "AS", "Adulto sin identificacion" )
                        , ( "MS", "Menor sin identificacion" )
                        , ( "PS", "Pasaporte" )
                        , ( "RC", "Registro Civil" )
                        ]
                    }
                }

        fechaNacimientoField =
            Form.textField
                { parser =
                    \str ->
                        String.split "/" str
                            |> (\lst ->
                                    if List.length lst /= 3 then
                                        Err "No es una fecha valida"

                                    else
                                        Ok (Time.millisToPosix 0)
                               )
                , value = .fechaNacimiento
                , update = \value values -> { values | fechaNacimiento = value }
                , attributes = { label = "Fecha de Nacimiento", placeholder = "Fecha de Nacimiento" }
                }

        telefonoField =
            Form.textField
                { parser = Ok
                , value = .telefono
                , update = \value values -> { values | telefono = value }
                , attributes = { label = "Telefono", placeholder = "Telefono" }
                }

        residenciaPaisField =
            Form.textField
                { parser = Ok
                , value = .residenciaPais
                , update = \value values -> { values | residenciaPais = value }
                , attributes = { label = "Pais de Residencia", placeholder = "Pais de Residencia" }
                }

        residenciaDepartamentoField =
            Form.textField
                { parser = Ok
                , value = .residenciaDepartamento
                , update = \value values -> { values | residenciaDepartamento = value }
                , attributes = { label = "Departamento de Residencia", placeholder = "Departamento de Residencia" }
                }

        residenciaMunicipioField =
            Form.textField
                { parser = Ok
                , value = .residenciaMunicipio
                , update = \value values -> { values | residenciaMunicipio = value }
                , attributes = { label = "Municipio de Residencia", placeholder = "Municipio de Residencia" }
                }

        residenciaBarrioField =
            Form.textField
                { parser = Ok
                , value = .residenciaBarrio
                , update = \value values -> { values | residenciaBarrio = value }
                , attributes = { label = "Barrio de Residencia", placeholder = "Barrio de Residencia" }
                }

        residenciaDireccionField =
            Form.textField
                { parser = Ok
                , value = .residenciaDireccion
                , update = \value values -> { values | residenciaDireccion = value }
                , attributes = { label = "Direccion de Residencia", placeholder = "Direccion de Residencia" }
                }
    in
    Form.succeed Output
        |> Form.append nombresField
        |> Form.append apellidosField
        |> Form.append generoField
        |> Form.append documentoNumeroField
        |> Form.append documentoTipoField
        |> Form.append fechaNacimientoField
        |> Form.append telefonoField
        |> Form.append residenciaPaisField
        |> Form.append residenciaDepartamentoField
        |> Form.append residenciaMunicipioField
        |> Form.append residenciaBarrioField
        |> Form.append residenciaDireccionField


prellenar : Paciente -> Values
prellenar { nombres, apellidos, genero, documentoTipo, documentoNumero, fechaNacimiento, telefono, residenciaPais, residenciaDepartamento, residenciaMunicipio, residenciaBarrio, residenciaDireccion } =
    { nombres = Value.filled nombres
    , apellidos = Value.filled apellidos
    , genero = Value.filled genero
    , documentoNumero = Value.filled documentoNumero
    , documentoTipo = Value.filled documentoTipo
    , fechaNacimiento = Value.filled ""
    , telefono = Value.filled telefono
    , residenciaPais = Value.filled residenciaPais
    , residenciaDepartamento = Value.filled residenciaDepartamento
    , residenciaMunicipio = Value.filled residenciaMunicipio
    , residenciaBarrio = Value.filled residenciaBarrio
    , residenciaDireccion = Value.filled residenciaDireccion
    }
