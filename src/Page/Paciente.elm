module Page.Paciente exposing (Model, Msg, init, update, view)

import Data.Paciente exposing (Output)
import Data.Session exposing (Session)
import Form exposing (Form)
import Form.Value as Value exposing (Value)
import Form.View
import Html exposing (Html)
import Html.Attributes as Attributes
import Http
import Page.PacienteForm exposing (Values, form)
import Request.Paciente exposing (crear, editar, obtener, obtenerLista)


type Model
    = FillingForm (Form.View.Model Values)


type Msg
    = FormChanged (Form.View.Model Values)
    | PacienteListo Output
    | PacienteListoHttp (Result Http.Error String)


init : Model
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
        |> Form.View.idle
        |> FillingForm


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        FormChanged newForm ->
            ( FillingForm newForm, Cmd.none )

        PacienteListo output ->
            ( model, crear session output PacienteListoHttp )

        PacienteListoHttp (Ok _) ->
            ( model, Cmd.none )

        PacienteListoHttp (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        FillingForm formModel ->
            Html.div [ Attributes.class "container" ]
                [ Html.h2 [ Attributes.class "title" ] [ Html.text "Crear Paciente" ]
                , Form.View.asHtml
                    { onChange = FormChanged
                    , action = "Crear Paciente"
                    , loading = "Loading..."
                    , validation = Form.View.ValidateOnSubmit
                    }
                    (Form.map PacienteListo form)
                    formModel
                ]
