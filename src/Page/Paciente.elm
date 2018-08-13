module Page.Paciente exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Data.HistoriaUrgencias exposing (..)
import Data.Paciente exposing (Paciente)
import Form exposing (Form)
import Form.Value as Value exposing (Value)
import Page.BulmaForm as Bulma


type Model
    = FillingForm (Bulma.Model Values)


type Msg
    = FormChanged (Bulma.Model Values)
    | PacienteListo Output


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


type alias Output =
    { nombres : String
    , apellidos : String
    , genero : String
    , documentoNumero : String
    , documentoTipo : String
    , fechaNacimiento : String
    , telefono : String
    , residenciaPais : String
    , residenciaDepartamento : String
    , residenciaMunicipio : String
    , residenciaBarrio : String
    , residenciaDireccion : String
    }


init =
    { nombres = Value.blank
    , apellidos = Value.blank
    , genero = Value.blank
    , documentoNumero = Value.blank
    , documentoTipo = Value.blank
    , fechaNacimiento = Value.blank
    , telefono = Value.blank
    , residenciaPais = Value.blank
    , residenciaDepartamento = Value.blank
    , residenciaMunicipio = Value.blank
    , residenciaBarrio = Value.blank
    , residenciaDireccion = Value.blank
    }
        |> Bulma.idle
        |> FillingForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormChanged newForm ->
            case model of
                FillingForm _ ->
                    ( FillingForm newForm, Cmd.none )

        PacienteListo output ->
            ( model, Cmd.none ) |> Debug.log (toString output)


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
                { parser = Ok
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
                    , options = [ ( "F", "Femenino" ), ( "M", "Masculino" ), ( "I", "Indeterminado" ) ]
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
            Form.textField
                { parser = Ok
                , value = .documentoTipo
                , update = \value values -> { values | documentoTipo = value }
                , attributes = { label = "Tipo Documento", placeholder = "Tipo Documento" }
                }

        fechaNacimientoField =
            Form.textField
                { parser = Ok
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
                , attributes = { label = "residenciaPais", placeholder = "Nombres" }
                }

        residenciaDepartamentoField =
            Form.textField
                { parser = Ok
                , value = .residenciaDepartamento
                , update = \value values -> { values | residenciaDepartamento = value }
                , attributes = { label = "residenciaDepartamento", placeholder = "Nombres" }
                }

        residenciaMunicipioField =
            Form.textField
                { parser = Ok
                , value = .residenciaMunicipio
                , update = \value values -> { values | residenciaMunicipio = value }
                , attributes = { label = "residenciaMunicipio", placeholder = "Nombres" }
                }

        residenciaBarrioField =
            Form.textField
                { parser = Ok
                , value = .residenciaBarrio
                , update = \value values -> { values | residenciaBarrio = value }
                , attributes = { label = "residenciaBarrio", placeholder = "Nombres" }
                }

        residenciaDireccionField =
            Form.textField
                { parser = Ok
                , value = .residenciaDireccion
                , update = \value values -> { values | residenciaDireccion = value }
                , attributes = { label = "residenciaDireccion", placeholder = "Nombres" }
                }
    in
        Form.succeed Output
            |> Form.append nombresField
            |> Form.append apellidosField
            |> Form.append generoField
            |> Form.append documentoTipoField
            |> Form.append documentoNumeroField
            |> Form.append fechaNacimientoField
            |> Form.append telefonoField
            |> Form.append residenciaPaisField
            |> Form.append residenciaDepartamentoField
            |> Form.append residenciaMunicipioField
            |> Form.append residenciaBarrioField
            |> Form.append residenciaDireccionField


view : Model -> Html Msg
view model =
    case model of
        FillingForm formModel ->
            Html.div [ class "container" ]
                [ Html.h2 [ class "title" ] [ Html.text "Crear Paciente" ]
                , Bulma.asHtml
                    { onChange = FormChanged
                    , action = "Crear Paciente"
                    , loading = "Loading..."
                    , validation = Bulma.ValidateOnSubmit
                    }
                    (Form.map PacienteListo form)
                    formModel
                ]
