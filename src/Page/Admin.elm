module Page.Admin exposing (..)

import Navigation exposing (Location, newUrl)
import Html exposing (Html, text, div, input, label, button)
import Html.Attributes exposing (src, class, type_, value, name, defaultValue)
import Html.Events exposing (onInput, onClick)
import Json.Encode as Encode
import Json.Decode as Decode exposing (field)
import Http
import Ports
import Page.HistoriaUrgencias
import Data.HistoriaUrgencias exposing (..)
import Data.Paciente exposing (..)
import Date exposing (..)
import Task exposing (perform)
import Request.HistoriaUrgencias
import Request.Admin
import Validate exposing (validate)
import Data.Usuario exposing (..)


maybeEmpty : Maybe Usuario -> Usuario
maybeEmpty x =
    Maybe.withDefault (Usuario "" "" "" "" "") x


type AdminPage
    = CrearUsuarioVista
    | ListaUsuarioVista
    | ActualizarUsuarioVista


type VistasMedicos
    = VistaCrearPaciente
    | VistaBuscarPacientes
    | VistaCrearHistoria


type alias Model =
    { username : Maybe String
    , route : AdminPage
    , grupo : Maybe String
    , usernameInput : String
    , passwordInput : String
    , error : String
    , authorization : Maybe String
    , porNombre : String
    , listaUsuarios : List Usuario
    , pacienteActual : Maybe Paciente
    , historiaUrgencias : Data.HistoriaUrgencias.Model
    , usuarioEditar : Maybe Usuario
    , usuarioCrear : UsuarioCrear
    , pacienteCrear : Paciente
    , pacienteBuscar : { nombres : String, apellidos : String, documentoNumero : String }
    , listaPacientes : List Paciente
    }


inicial : Model
inicial =
    { username = Nothing
    , route = ListaUsuarioVista
    , grupo = Nothing
    , usernameInput = ""
    , passwordInput = ""
    , error = ""
    , authorization = Nothing
    , porNombre = ""
    , listaUsuarios = []
    , pacienteActual = Nothing
    , historiaUrgencias = initialHistoriaUrgencias
    , usuarioEditar = Nothing
    , usuarioCrear = UsuarioCrear "" "" "" ""
    , pacienteCrear = initialPaciente
    , pacienteBuscar = { nombres = "", apellidos = "", documentoNumero = "" }
    , listaPacientes = []
    }


type Msg
    = NoOp String
    | Salir
      -- Autenticacion
    | Autenticar
    | AutenticarServer (Result Http.Error Credentials)
    | DefinirNombreUsuario String
    | DefinirPassword String
      -- Lista de Usuarios
    | SolicitarUsuarios
    | SolicitarUsuariosHttp (Result Http.Error (List Usuario))
      -- historia
    | SolicitarHistoriaUrgencias
    | SolicitarHistoriaUrgenciasHttp (Result Http.Error Data.HistoriaUrgencias.Model)
    | SendStringToJs
    | SetCredentials Credentials
    | EditarUsuario Usuario
      -- Crear Usuario
    | DefinirNombreUsuarioCrear String
    | DefinirGrupoCrear String
    | DefinirCorreoElectronicoCrear String
    | DefinirPalabraClaveCrear String
    | EnviarCrearUsuario
    | EnviarCrearUsuarioHttp (Result Http.Error String)
      -- Crear Perfiles (Pacientes)
    | DefinirCampoPaciente (String -> Paciente -> Paciente) String
    | EnviarCrearPaciente
    | EnviarCrearPacienteHttp (Result Http.Error String)
      -- Solicitar Perfiles (Pacientes)
    | DefinirNombresBuscar String
    | DefinirApellidosBuscar String
    | DefinirDocumentoBuscar String
    | EnviarBuscarPaciente
    | EnviarBuscarPacienteHttp (Result Http.Error (List Paciente))
    | SeleccionarPacienteParaHistoria Paciente
      -- Editar Historia Urgencias
    | DefinirFechaInicioConsulta Date
    | HistoriaUrgenciasMsg Page.HistoriaUrgencias.Msg
    | EnviarGuardarHistoria
    | EnviarGuardarHistoriaHttp (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NoOp _ ->
            model ! []

        Salir ->
            ( { model
                | username = Nothing
                , grupo = Nothing
                , authorization = Nothing
              }
            , Ports.toJs "salir"
            )

        DefinirNombreUsuario nuevo ->
            ( { model | usernameInput = nuevo }, Cmd.none )

        DefinirPassword nuevo ->
            ( { model | passwordInput = nuevo }, Cmd.none )

        Autenticar ->
            ( model, sendCredentials model )

        AutenticarServer (Ok key) ->
            ( { model
                | username = key.username
                , grupo = key.grupo
                , authorization = key.authorization
              }
            , Cmd.batch
                [ Request.Admin.solicitarUsuarios (Maybe.withDefault "" model.authorization) SolicitarUsuariosHttp
                , Ports.storeSession (Maybe.withDefault "" key.authorization)
                , if key.grupo == Just "ADMIN" then
                    newUrl "/#admin"
                  else
                    newUrl <| "/#medico/" ++ Maybe.withDefault "" key.username
                ]
            )
                |> Debug.log (toString key)

        AutenticarServer (Err err) ->
            ( { model | error = "No existe el usuario o la clave es incorrecta." }
                |> Debug.log (toString err)
            , Cmd.none
            )

        SolicitarUsuarios ->
            ( model, Request.Admin.solicitarUsuarios (Maybe.withDefault "" model.authorization) SolicitarUsuariosHttp )

        SolicitarUsuariosHttp (Ok listaUsuarios) ->
            ( { model | listaUsuarios = listaUsuarios }
                |> Debug.log (toString listaUsuarios)
            , Cmd.none
            )

        SolicitarUsuariosHttp (Err err) ->
            ( model |> Debug.log (toString err), Cmd.none )

        SolicitarHistoriaUrgencias ->
            ( model, Request.HistoriaUrgencias.solicitar SolicitarHistoriaUrgenciasHttp (Maybe.withDefault "" model.authorization) )

        SolicitarHistoriaUrgenciasHttp (Ok historiaUrgencias) ->
            ( { model | historiaUrgencias = historiaUrgencias }
                |> Debug.log (toString historiaUrgencias)
            , Cmd.none
            )

        SolicitarHistoriaUrgenciasHttp (Err err) ->
            ( model |> Debug.log (toString err), Cmd.none )

        SendStringToJs ->
            ( model, Ports.toJs "Victor" )

        SetCredentials key ->
            ( { model
                | username = key.username
                , grupo = key.grupo
                , authorization = key.authorization
              }
            , if key.grupo == Just "ADMIN" then
                Request.Admin.solicitarUsuarios (Maybe.withDefault "" model.authorization) SolicitarUsuariosHttp
              else
                Cmd.none
            )

        EditarUsuario usr ->
            ( { model | usuarioEditar = Just usr }, newUrl <| "#editar/" ++ usr.id )

        DefinirNombreUsuarioCrear str ->
            ( { model
                | usuarioCrear =
                    model.usuarioCrear
                        |> definirUsuario str
              }
            , Cmd.none
            )

        DefinirGrupoCrear str ->
            ( { model
                | usuarioCrear =
                    model.usuarioCrear
                        |> definirGrupo str
              }
            , Cmd.none
            )

        DefinirCorreoElectronicoCrear str ->
            ( { model
                | usuarioCrear =
                    model.usuarioCrear
                        |> definirCorreoElectronico str
              }
            , Cmd.none
            )

        DefinirPalabraClaveCrear str ->
            ( { model
                | usuarioCrear =
                    model.usuarioCrear
                        |> definirPalabraClave str
              }
            , Cmd.none
            )

        EnviarCrearUsuario ->
            ( model, Request.Admin.crearUsuario { authorization = (Maybe.withDefault "" model.authorization), usuarioCrear = model.usuarioCrear } EnviarCrearUsuarioHttp )

        EnviarCrearUsuarioHttp (Ok id) ->
            ( model, newUrl "#usuarios" )

        EnviarCrearUsuarioHttp (Err err) ->
            ( model, Cmd.none )

        --- Crear Perfiles ---
        DefinirCampoPaciente func str ->
            ( { model | pacienteCrear = model.pacienteCrear |> func str }, Cmd.none )

        -- TODO falta el apropiado manejo de errores y la notificacion de que fue
        -- exitosa la creacion del paciente
        EnviarCrearPaciente ->
            ( model, crearPaciente model )

        EnviarCrearPacienteHttp (Ok id) ->
            ( model, Cmd.none )

        EnviarCrearPacienteHttp (Err err) ->
            ( model, Cmd.none )

        -- TODO obtener lista de perfiles
        DefinirNombresBuscar nuevo ->
            let
                paciente =
                    model.pacienteBuscar

                nuevoPaciente =
                    { paciente | nombres = nuevo }
            in
                ( { model | pacienteBuscar = nuevoPaciente }, Cmd.none )

        DefinirApellidosBuscar nuevo ->
            let
                paciente =
                    model.pacienteBuscar

                nuevoPaciente =
                    { paciente | apellidos = nuevo }
            in
                ( { model | pacienteBuscar = nuevoPaciente }, Cmd.none )

        DefinirDocumentoBuscar nuevo ->
            let
                paciente =
                    model.pacienteBuscar

                nuevoPaciente =
                    { paciente | documentoNumero = nuevo }
            in
                ( { model | pacienteBuscar = nuevoPaciente }, Cmd.none )

        EnviarBuscarPaciente ->
            ( model, buscarPacientes model )

        EnviarBuscarPacienteHttp (Ok pct) ->
            ( { model | listaPacientes = pct }, Cmd.none )

        EnviarBuscarPacienteHttp (Err err) ->
            ( model, Cmd.none )

        SeleccionarPacienteParaHistoria pct ->
            let
                historia =
                    model.historiaUrgencias

                nuevaHistoria =
                    { historia
                        | paciente = pct.id
                        , medico = Maybe.withDefault "" model.username
                    }
            in
                ( { model
                    | pacienteActual = Just pct
                    , historiaUrgencias = nuevaHistoria
                  }
                , Cmd.none
                )

        -- Editar Historia de Urgencias
        DefinirFechaInicioConsulta tm ->
            let
                historia =
                    model.historiaUrgencias

                nuevaHistoria =
                    { historia
                        | fechaInicio = formatDate tm
                        , fechaFinalizacion = formatDate tm
                    }
            in
                ( { model | historiaUrgencias = nuevaHistoria }, Cmd.none ) |> Debug.log (toString tm)

        HistoriaUrgenciasMsg subMsg ->
            let
                ( subModel, cmdMsg ) =
                    Page.HistoriaUrgencias.update subMsg model.historiaUrgencias
            in
                ( { model | historiaUrgencias = subModel }, Cmd.none )

        EnviarGuardarHistoria ->
            ( model, model.historiaUrgencias |> Request.HistoriaUrgencias.guardar EnviarGuardarHistoriaHttp (Maybe.withDefault "" model.authorization) )

        EnviarGuardarHistoriaHttp (Ok id) ->
            ( { model
                | pacienteActual = Nothing
                , historiaUrgencias = initialHistoriaUrgencias
              }
            , Cmd.none
            )

        EnviarGuardarHistoriaHttp (Err err) ->
            ( model, Cmd.none )


adminVista : Model -> AdminPage -> Html Msg
adminVista model page =
    case page of
        CrearUsuarioVista ->
            div [] [ crearUsuarioCuestionario model ]

        ListaUsuarioVista ->
            div [] [ viewNavBar model, viewUsers model ]

        ActualizarUsuarioVista ->
            div []
                [ viewNavBar model
                , div []
                    [ inputControl
                        "Nombre de Usuario"
                        "usuario"
                        (maybeEmpty model.usuarioEditar).usuario
                        NoOp
                    , inputControl
                        "Grupo"
                        "grupo"
                        (maybeEmpty model.usuarioEditar).grupo
                        NoOp
                    , inputControl
                        "Correo Electronico"
                        "correoElectronico"
                        (maybeEmpty model.usuarioEditar).correoElectronico
                        NoOp
                    ]
                ]


decodeValue : Decode.Value -> Msg
decodeValue x =
    let
        result =
            Decode.decodeValue decodeCredentials x
    in
        case result of
            Ok cred ->
                SetCredentials cred

            Err _ ->
                Salir


crearUsuarioCuestionario : Model -> Html Msg
crearUsuarioCuestionario model =
    div []
        [ inputControl
            "Nombre de Usuario"
            "usuario"
            ""
            DefinirNombreUsuarioCrear
        , inputControl
            "Grupo"
            "grupo"
            ""
            DefinirGrupoCrear
        , inputControl
            "Correo Electronico"
            "correoElectronico"
            ""
            DefinirCorreoElectronicoCrear
        , inputControl
            "Contrasena"
            "palabraClave"
            ""
            DefinirPalabraClaveCrear
        , button [ class "button is-primary", onClick EnviarCrearUsuario ] [ text "Crear" ]
        ]


inputControl : String -> String -> String -> (String -> Msg) -> Html Msg
inputControl labelText nameText valueText msg =
    div [ class "field" ]
        [ label [ class "label" ] [ text labelText ]
        , div [ class "control" ]
            [ input
                [ type_ "text"
                , class "input"
                , name nameText
                , Html.Attributes.id nameText
                , defaultValue valueText
                , onInput msg
                ]
                []
            ]
        ]


textareaControl : String -> String -> String -> (String -> Msg) -> Html Msg
textareaControl labelText nameText valueText msg =
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , Html.textarea
            [ class "form-control"
            , name nameText
            , Html.Attributes.id nameText
            , defaultValue valueText
            , onInput msg
            ]
            []
        ]


view : Model -> Html Msg
view model =
    adminVista model model.route



-- TODO


vistaMedicos : Model -> Html Msg
vistaMedicos model =
    div []
        [ viewNavBar model
        , div [ class "container" ]
            [ div [ class "row" ]
                [ Html.aside [ class "col-md-3 nav flex-column" ]
                    [ Html.a
                        [ class "nav-item nav-link", Html.Attributes.href "#" ]
                        [ text "Crear Paciente" ]
                    , Html.a
                        [ class "nav-item nav-link", Html.Attributes.href "#" ]
                        [ text "Buscar Paciente" ]
                    ]
                ]
            ]
        ]


vistaSeccionMedicos :
    { a
        | listaPacientes : List Paciente
        , pacienteActual : Maybe Paciente
        , historiaUrgencias : Data.HistoriaUrgencias.Model
    }
    -> VistasMedicos
    -> Html Msg
vistaSeccionMedicos model page =
    case page of
        VistaCrearPaciente ->
            Html.section [ class "col" ]
                -- TODO
                [ button [ class "btn btn-primary", onClick EnviarCrearPaciente ] [ text "Crear" ]
                ]

        VistaBuscarPacientes ->
            Html.section [ class "col" ]
                [ inputControl "Nombres" "buscarNombres" "" DefinirNombresBuscar
                , inputControl "Apellidos" "buscarApellidos" "" DefinirApellidosBuscar
                , inputControl "Documento" "buscarDocumento" "" DefinirDocumentoBuscar
                , button [ onClick EnviarBuscarPaciente, class "btn btn-warning" ] [ text "Buscar" ]
                , div [] <| List.map verPaciente model.listaPacientes
                ]

        VistaCrearHistoria ->
            Html.section [ class "col" ]
                [ Html.h3 [] [ text <| .nombres <| Maybe.withDefault initialPaciente model.pacienteActual ]
                , Page.HistoriaUrgencias.view model.historiaUrgencias |> Html.map HistoriaUrgenciasMsg
                , button [ onClick EnviarGuardarHistoria, class "btn btn-success" ] [ text "Guardar" ]
                , div [] <| List.map (\a -> text a) (validate Page.HistoriaUrgencias.modelValidator model.historiaUrgencias)
                ]


verPaciente : Paciente -> Html Msg
verPaciente pct =
    div [ class "card" ]
        [ div [ class "card-header" ] [ text <| pct.nombres ++ " ", text pct.apellidos ]
        , div [ class "card-body" ]
            [ Html.h5 [ class "card-title" ]
                [ text <| pct.documentoTipo ++ " ", text pct.documentoNumero ]
            , button [ class "btn btn-success", onClick (SeleccionarPacienteParaHistoria pct) ]
                [ text "Iniciar Consulta Urgencias" ]
            ]
        ]


viewUsers model =
    let
        viewUser a =
            Html.tr [ onClick (EditarUsuario a) ]
                [ Html.td [] [ text a.usuario ]
                , Html.td [] [ text a.grupo ]
                , Html.td [] [ text a.correoElectronico ]
                , Html.td [] [ text a.fechaCreacion ]
                ]
    in
        div []
            [ Html.table [ class "table" ] <| List.map viewUser model.listaUsuarios
            , Html.a
                [ onClick SolicitarUsuarios
                , class "button is-primary"
                ]
                [ text "Actualizar Lista Usuarios" ]
            , Html.a
                [ class "button is-dark"
                , Html.Attributes.href "#crear"
                ]
                [ text "Crear Usuario" ]
            ]


viewLogin model =
    div []
        [ inputControl "Nombre de usuario" "username" "" DefinirNombreUsuario
        , inputControl "Clave" "password" "" DefinirPassword
        , button [ onClick Autenticar, class "btn btn-outline-primary", type_ "submit" ] [ text "Ingresar" ]
        , Html.small [ class "form-text" ] [ text model.error ]
        ]


viewNavBar model =
    Html.nav [ class "navbar navbar-expand-lg navbar-dark bg-dark" ]
        [ div [ class "navbar-brand" ] [ text <| Maybe.withDefault "" model.username ]
        , div [ class "navbar-nav" ] [ Html.a [ class "nav-item nav-link", onClick Salir ] [ text "Salir" ] ]
        ]



-- Encoders --


credentialsEncoder : Model -> Encode.Value
credentialsEncoder model =
    Encode.object
        [ ( "username", Encode.string model.usernameInput )
        , ( "password", Encode.string model.passwordInput )
        ]


type alias Credentials =
    { username : Maybe String
    , grupo : Maybe String
    , authorization : Maybe String
    }


decodeCredentials : Decode.Decoder Credentials
decodeCredentials =
    Decode.map3 Credentials
        (field "username" (Decode.nullable Decode.string))
        (field "grupo" (Decode.nullable Decode.string))
        (field "authorization" (Decode.nullable Decode.string))



-- Http --


sendCredentials : Model -> Cmd Msg
sendCredentials model =
    let
        server =
            "http://localhost:8070/login"
    in
        Http.send AutenticarServer <|
            Http.request
                { method = "POST"
                , headers =
                    [ Http.header "Content-Type" "application/json"
                    ]
                , url = server
                , body = (credentialsEncoder model |> Http.jsonBody)
                , expect = Http.expectJson decodeCredentials
                , timeout = Nothing
                , withCredentials = False
                }


crearPaciente : Model -> Cmd Msg
crearPaciente model =
    let
        server =
            "http://localhost:8070/perfiles"
    in
        Http.send EnviarCrearPacienteHttp <|
            Http.request
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" (Maybe.withDefault "" model.authorization)
                    , Http.header "Content-Type" "application/json"
                    ]
                , url = server
                , body = encodePaciente model.pacienteCrear |> Http.jsonBody
                , expect = Http.expectJson Decode.string
                , timeout = Nothing
                , withCredentials = False
                }


buscarPacientes : Model -> Cmd Msg
buscarPacientes model =
    let
        server =
            "http://localhost:8070/perfiles?nombres=" ++ model.pacienteBuscar.nombres
    in
        Http.send EnviarBuscarPacienteHttp <|
            Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Authorization" (Maybe.withDefault "" model.authorization)
                    , Http.header "Content-Type" "application/json"
                    ]
                , url = server
                , body = Http.emptyBody
                , expect = Http.expectJson (Decode.list decodePaciente)
                , timeout = Nothing
                , withCredentials = False
                }


getTime : Cmd Msg
getTime =
    Task.perform DefinirFechaInicioConsulta Date.now


formatDate : Date -> String
formatDate date =
    let
        yy =
            year date |> toString |> agregarZero

        mm =
            formatoMes date

        dd =
            day date |> toString |> agregarZero

        hh =
            hour date |> toString |> agregarZero

        min =
            minute date |> toString |> agregarZero

        ss =
            second date |> toString |> agregarZero
    in
        (String.join "-" [ yy, mm, dd ]) ++ "T" ++ (String.join ":" [ hh, min, ss ]) ++ "-05:00"


agregarZero : String -> String
agregarZero str =
    if String.length str == 1 then
        "0" ++ str
    else
        str


formatoMes : Date -> String
formatoMes date =
    case month date of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"
