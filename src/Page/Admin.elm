module Page.Admin exposing
    ( AdminVista(..)
    , Model
    , Msg(..)
    , VistasMedicos(..)
    , adminVista
    , crearUsuarioCuestionario
    , editarUsuarioVista
    , init
    , update
    , view
    , viewUsers
    )

import Data.Session exposing (..)
import Data.Usuario exposing (..)
import Helpers exposing (inputControl)
import Html exposing (Html, a, button, div, h2, input, label, p, span, text)
import Html.Attributes exposing (class, name, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Request.Usuario exposing (crear, editar, solicitar)


type AdminVista
    = CrearUsuarioVista
    | ListaUsuarioVista
    | EditarUsuarioVista Usuario


type VistasMedicos
    = VistaCrearPaciente
    | VistaBuscarPacientes
    | VistaCrearHistoria


type alias Model =
    { vista : AdminVista
    , porNombre : String
    , listaUsuarios : List Usuario
    , usuarioCrear : UsuarioCrear
    , errorUsuario : Maybe String
    , error : String
    }


init : Session -> ( Model, Cmd Msg )
init { autorizacion } =
    ( { vista = ListaUsuarioVista
      , porNombre = ""
      , listaUsuarios = []
      , usuarioCrear = UsuarioCrear "" "" "" ""
      , errorUsuario = Nothing
      , error = ""
      }
    , Cmd.batch [ solicitar autorizacion SolicitarUsuariosHttp ]
    )


type Msg
    = NoOp String
      -- Lista de Usuarios
    | SolicitarUsuarios
    | SolicitarUsuariosHttp (Result Http.Error (List Usuario))
    | EditarUsuario Usuario
      -- Crear Usuario
    | DefinirNombreUsuarioCrear String
    | DefinirGrupoCrear String
    | DefinirCorreoElectronicoCrear String
    | DefinirPalabraClaveCrear String
    | EnviarCrearUsuario
    | EnviarCrearUsuarioHttp (Result Http.Error String)
    | EnviarEditarUsuario Usuario
    | EnviarEditarUsuarioHttp (Result Http.Error String)
    | IniciarCrearUsuario
    | EditarGrupoUsr String
    | EditarCorreoUsr String
    | VerListaUsuarios


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session action model =
    case action of
        NoOp _ ->
            ( model, Cmd.none )

        SolicitarUsuarios ->
            ( model, solicitar session.autorizacion SolicitarUsuariosHttp )

        SolicitarUsuariosHttp (Ok listaUsuarios) ->
            ( { model | listaUsuarios = listaUsuarios }
            , Cmd.none
            )

        SolicitarUsuariosHttp (Err err) ->
            case err of
                Http.BadPayload str _ ->
                    ( { model | error = str }, Cmd.none )

                Http.BadStatus _ ->
                    ( { model | error = "Bad Status" }, Cmd.none )

                Http.NetworkError ->
                    ( { model | error = "No conexion" }, Cmd.none )

                Http.Timeout ->
                    ( { model | error = "Se demoro mucho" }, Cmd.none )

                Http.BadUrl _ ->
                    ( { model | error = "Url mal" }, Cmd.none )

        EditarUsuario usr ->
            ( { model | vista = EditarUsuarioVista usr }, Cmd.none )

        DefinirNombreUsuarioCrear str ->
            ( { model | usuarioCrear = definirUsuario str model.usuarioCrear }
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
            ( model
            , crear
                { autorizacion = session.autorizacion, usuarioCrear = model.usuarioCrear }
                EnviarCrearUsuarioHttp
            )

        EnviarCrearUsuarioHttp (Ok id) ->
            ( { model | vista = ListaUsuarioVista }, Cmd.none )

        EnviarCrearUsuarioHttp (Err err) ->
            case err of
                Http.BadStatus str ->
                    ( { model | errorUsuario = Just "Ya existe usuario" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EnviarEditarUsuario usr ->
            ( model
            , editar
                { autorizacion = session.autorizacion, usuario = usr }
                EnviarEditarUsuarioHttp
            )

        EnviarEditarUsuarioHttp (Ok id) ->
            ( { model | vista = ListaUsuarioVista }, solicitar session.autorizacion SolicitarUsuariosHttp )

        EnviarEditarUsuarioHttp (Err err) ->
            ( model, Cmd.none )

        IniciarCrearUsuario ->
            ( { model | vista = CrearUsuarioVista }, Cmd.none )

        VerListaUsuarios ->
            ( { model | vista = ListaUsuarioVista }, Cmd.none )

        EditarGrupoUsr str ->
            case model.vista of
                EditarUsuarioVista usuario ->
                    let
                        nuevoUsuario =
                            { usuario | grupo = str }
                    in
                    ( { model | vista = EditarUsuarioVista nuevoUsuario }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditarCorreoUsr str ->
            case model.vista of
                EditarUsuarioVista usuario ->
                    let
                        nuevoUsuario =
                            { usuario | correoElectronico = str }
                    in
                    ( { model | vista = EditarUsuarioVista nuevoUsuario }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


adminVista : Model -> AdminVista -> Html Msg
adminVista model page =
    case page of
        CrearUsuarioVista ->
            div [] [ crearUsuarioCuestionario model ]

        ListaUsuarioVista ->
            div [ class "container" ] [ viewUsers model.listaUsuarios ]

        EditarUsuarioVista usr ->
            editarUsuarioVista usr


editarUsuarioVista : Usuario -> Html Msg
editarUsuarioVista usr =
    div [ class "column is-6 is-offset-3" ]
        [ h2 [ class "title" ] [ text "Editar Usuario" ]
        , h2 [ class "subtitle" ] [ text usr.usuario ]
        , inputControl
            "Grupo"
            "grupo"
            usr.grupo
            Nothing
            EditarGrupoUsr
        , inputControl
            "Correo Electronico"
            "correoElectronico"
            usr.correoElectronico
            Nothing
            EditarCorreoUsr
        , div [ class "buttons" ]
            [ button [ class "button is-primary", onClick (EnviarEditarUsuario usr) ] [ text "Guardar" ]
            , button [ class "button is-danger", onClick VerListaUsuarios ] [ text "Cancelar" ]
            ]
        ]


crearUsuarioCuestionario : Model -> Html Msg
crearUsuarioCuestionario model =
    div [ class "container" ]
        [ div [ class "column is-6 is-offset-3" ]
            [ h2 [ class "title" ] [ text "Crear Usuario" ]
            , inputControl
                "Nombre de Usuario"
                "usuario"
                model.usuarioCrear.usuario
                model.errorUsuario
                DefinirNombreUsuarioCrear
            , inputControl
                "Grupo"
                "grupo"
                model.usuarioCrear.grupo
                Nothing
                DefinirGrupoCrear
            , inputControl
                "Correo Electronico"
                "correoElectronico"
                model.usuarioCrear.correoElectronico
                Nothing
                DefinirCorreoElectronicoCrear
            , inputControl
                "Contrasena"
                "palabraClave"
                model.usuarioCrear.palabraClave
                Nothing
                DefinirPalabraClaveCrear
            , div [ class "buttons" ]
                [ button [ class "button is-primary", onClick EnviarCrearUsuario ] [ text "Crear" ]
                , Html.a [ class "button is-danger", onClick VerListaUsuarios ] [ text "Cancelar" ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    adminVista model model.vista


viewUsers : List Usuario -> Html Msg
viewUsers usuarios =
    let
        viewUser a =
            Html.tr [ onClick (EditarUsuario a) ]
                [ Html.td [] [ text a.usuario ]
                , Html.td [] [ text a.grupo ]
                , Html.td [] [ text a.correoElectronico ]
                , Html.td [] [ String.fromInt a.fechaCreacion |> text ]
                , Html.td [] [ Html.span [ class "icon" ] [ Html.i [ class "fas fa-edit" ] [] ] ]
                ]
    in
    div []
        [ Html.table [ class "table is-hoverable" ] <|
            List.map viewUser usuarios
        , div [ class "buttons" ]
            [ Html.button
                [ class "button is-primary"
                , onClick IniciarCrearUsuario
                ]
                [ text "Crear Usuario" ]
            ]
        ]
