module Page.Medico exposing (Model, Msg, init, update, view)

import Data.Paciente exposing (Paciente)
import Data.Session exposing (Session)
import Helpers exposing (inputControl)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Request.Paciente exposing (obtenerLista)


type alias Model =
    { porNombres : String
    , porApellidos : String
    , porDocumentoNumero : String
    , resultadoPacientes : List Paciente
    }


init : Model
init =
    { porNombres = ""
    , porApellidos = ""
    , porDocumentoNumero = ""
    , resultadoPacientes = []
    }



-- Update


type Msg
    = CambiarCampo Campo String
    | BuscarPaciente
    | ResultadoBusqueda (Result Http.Error (List Paciente))


type alias ParametrosBusqueda a =
    { a
        | porNombres : String
        , porApellidos : String
        , porDocumentoNumero : String
    }


type Campo
    = PorNombres
    | PorApellidos
    | PorDocumentoNumero


cambiarCampo : String -> Campo -> ParametrosBusqueda a -> ParametrosBusqueda a
cambiarCampo nuevo campo parametros =
    case campo of
        PorNombres ->
            { parametros | porNombres = nuevo }

        PorApellidos ->
            { parametros | porApellidos = nuevo }

        PorDocumentoNumero ->
            { parametros | porDocumentoNumero = nuevo }


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        CambiarCampo campo string ->
            ( cambiarCampo string campo model, Cmd.none )

        BuscarPaciente ->
            ( model, obtenerLista session model ResultadoBusqueda )

        ResultadoBusqueda (Ok resultado) ->
            ( { model | resultadoPacientes = resultado }, Cmd.none )

        ResultadoBusqueda (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view { resultadoPacientes } =
    Html.section [ Attributes.class "column" ]
        [ inputControl "Nombres" "buscarNombres" "" Nothing (CambiarCampo PorNombres)
        , inputControl "Apellidos" "buscarApellidos" "" Nothing (CambiarCampo PorApellidos)
        , inputControl "Documento" "buscarDocumento" "" Nothing (CambiarCampo PorDocumentoNumero)
        , Html.button [ Attributes.class "button is-primary", Events.onClick BuscarPaciente ] [ Html.text "Buscar" ]
        , Html.div [ Attributes.class "container" ] <| List.map verPaciente resultadoPacientes
        ]


verPaciente : Paciente -> Html Msg
verPaciente pct =
    Html.div [ Attributes.class "column is-6" ]
        [ Html.div [ Attributes.class "card" ]
            [ Html.div [ Attributes.class "card-header" ]
                [ Html.h4 [ Attributes.class "card-header-title" ]
                    [ Html.text <| pct.nombres ++ " ", Html.text pct.apellidos ]
                ]
            , Html.div [ Attributes.class "card-content" ]
                [ Html.h5 [ Attributes.class "subtitle" ]
                    [ Html.text <| pct.documentoTipo ++ " ", Html.text pct.documentoNumero ]
                , Html.p [] [ Html.text <| "Fecha de Nacimiento: " ++ Format.format "%Y/%m/%d" pct.fechaNacimiento ]
                , Html.p [] [ Html.text <| "Genero: " ++ pct.genero ]
                ]
            , Html.div [ Attributes.class "card-footer" ]
                [ Html.span [ Attributes.class "card-footer-item" ]
                    [ Html.a [ Attributes.href <| "#editar/" ++ pct.id ] [ Html.text "Editar" ] ]
                , Html.span [ Attributes.class "card-footer-item" ]
                    [ Html.a [] [ Html.text "Iniciar Consulta" ] ]
                ]
            ]
        ]
