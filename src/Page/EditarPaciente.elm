module Page.EditarPaciente exposing (Model, Msg(..), init, update, view)

import Html exposing (Html)
import Html.Attributes as Attributes
import Page.BulmaForm as Bulma
import Data.Paciente exposing (Output, Paciente)
import Request.Paciente exposing (editar, obtener)
import Http
import Date
import Data.Session exposing (Session)
import Date.Format as Format
import Page.PacienteForm exposing (form, Values, prellenar)
import Form.Value as Value exposing (Value)
import Form exposing (Form)


type alias Model =
    { aEditar : Bulma.Model Values, id : String }


type Msg
    = FormChanged (Bulma.Model Values)
    | ResultadoObtenerPaciente (Result Http.Error Paciente)
    | PacienteListo Output
    | PacienteListoHttp (Result Http.Error String)


init : Model
init =
    { aEditar =
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
    , id = ""
    }


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        FormChanged newForm ->
            ( { model | aEditar = newForm }, Cmd.none )

        ResultadoObtenerPaciente (Ok paciente) ->
            ( { model | aEditar = Bulma.idle <| prellenar paciente, id = paciente.id }, Cmd.none )

        ResultadoObtenerPaciente (Err _) ->
            ( model, Cmd.none )

        PacienteListo output ->
            ( model, editar session model.id output PacienteListoHttp )

        PacienteListoHttp (Ok _) ->
            ( model, Cmd.none )

        PacienteListoHttp (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view { aEditar } =
    Html.div [ Attributes.class "container" ]
        [ Html.h2 [ Attributes.class "title" ] [ Html.text "Editar Paciente" ]
        , Bulma.asHtml
            { onChange = FormChanged
            , action = "Guardar Cambios"
            , loading = "Loading..."
            , validation = Bulma.ValidateOnSubmit
            }
            (Form.map PacienteListo form)
            aEditar
        ]
