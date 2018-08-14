module Page.Paciente exposing (Model, Msg, init, update, view)

import Html exposing (Html)
import Html.Attributes as Attributes
import Form exposing (Form)
import Form.Value as Value exposing (Value)
import Page.BulmaForm as Bulma
import Data.Paciente exposing (Output)
import Request.Paciente exposing (editar, crear, obtener, obtenerLista)
import Page.PacienteForm exposing (form, Values)
import Http
import Date
import Data.Session exposing (Session)


type Model
    = FillingForm (Bulma.Model Values)


type Msg
    = FormChanged (Bulma.Model Values)
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
        |> Bulma.idle
        |> FillingForm


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        FormChanged newForm ->
            case model of
                FillingForm _ ->
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
                , Bulma.asHtml
                    { onChange = FormChanged
                    , action = "Crear Paciente"
                    , loading = "Loading..."
                    , validation = Bulma.ValidateOnSubmit
                    }
                    (Form.map PacienteListo form)
                    formModel
                ]
