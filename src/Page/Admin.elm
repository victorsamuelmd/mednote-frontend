module Page.Admin exposing (..)

import Html exposing (Html, text, div, input, label, button, a, span, h2, p)
import Html.Attributes exposing (src, class, type_, value, name, defaultValue)
import Html.Events exposing (onInput, onClick)
import Data.Session exposing (..)
import Http
import Request.Admin
import Data.Usuario exposing (..)


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
    }


inicial : Model
inicial =
    { vista = ListaUsuarioVista
    , porNombre = ""
    , listaUsuarios = []
    , usuarioCrear = UsuarioCrear "" "" "" ""
    , errorUsuario = Nothing
    }


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
            model ! []

        SolicitarUsuarios ->
            ( model, Request.Admin.solicitarUsuarios session.autorizacion SolicitarUsuariosHttp )

        SolicitarUsuariosHttp (Ok listaUsuarios) ->
            ( { model | listaUsuarios = listaUsuarios }
                |> Debug.log (toString listaUsuarios)
            , Cmd.none
            )

        SolicitarUsuariosHttp (Err err) ->
            ( model |> Debug.log (toString err), Cmd.none )

        EditarUsuario usr ->
            ( { model | vista = EditarUsuarioVista usr }, Cmd.none )

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
            ( model
            , Request.Admin.crearUsuario
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
            , Request.Admin.editar
                { autorizacion = session.autorizacion, usuario = usr }
                EnviarEditarUsuarioHttp
            )

        EnviarEditarUsuarioHttp (Ok id) ->
            ( { model | vista = ListaUsuarioVista }, Request.Admin.solicitarUsuarios session.autorizacion SolicitarUsuariosHttp )

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
            div [] [ crearUsuarioCuestionario model.errorUsuario ]

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
            [ a [ class "button is-primary", onClick (EnviarEditarUsuario usr) ] [ text "Guardar" ]
            , a [ class "button is-danger", onClick VerListaUsuarios ] [ text "Cancelar" ]
            ]
        ]


crearUsuarioCuestionario : Maybe String -> Html Msg
crearUsuarioCuestionario error =
    div [ class "container" ]
        [ div [ class "column is-6 is-offset-3" ]
            [ h2 [ class "title" ] [ text "Crear Usuario" ]
            , inputControl
                "Nombre de Usuario"
                "usuario"
                ""
                error
                DefinirNombreUsuarioCrear
            , inputControl
                "Grupo"
                "grupo"
                ""
                Nothing
                DefinirGrupoCrear
            , inputControl
                "Correo Electronico"
                "correoElectronico"
                ""
                Nothing
                DefinirCorreoElectronicoCrear
            , inputControl
                "Contrasena"
                "palabraClave"
                ""
                Nothing
                DefinirPalabraClaveCrear
            , div [ class "buttons" ]
                [ button [ class "button is-primary", onClick EnviarCrearUsuario ] [ text "Crear" ]
                , Html.a [ class "button is-danger", onClick VerListaUsuarios ] [ text "Cancelar" ]
                ]
            ]
        ]


inputControl : String -> String -> String -> Maybe String -> (String -> Msg) -> Html Msg
inputControl labelText nameText valueText err msg =
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
        , case err of
            Just error ->
                p [ class "help is-danger" ] [ text error ]

            Nothing ->
                text ""
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
    adminVista model model.vista


viewUsers : List Usuario -> Html Msg
viewUsers usuarios =
    let
        viewUser a =
            Html.tr [ onClick (EditarUsuario a) ]
                [ Html.td [] [ text a.usuario ]
                , Html.td [] [ text a.grupo ]
                , Html.td [] [ text a.correoElectronico ]
                , Html.td [] [ text a.fechaCreacion ]
                , Html.td [] [ Html.span [ class "icon" ] [ Html.i [ class "fas fa-edit" ] [] ] ]
                ]
    in
        div []
            [ Html.table [ class "table is-hoverable" ] <|
                List.map viewUser usuarios
            , div [ class "buttons" ]
                [ Html.a
                    [ class "button is-primary"
                    , onClick IniciarCrearUsuario
                    ]
                    [ text "Crear Usuario" ]
                ]
            ]



-- Encoders --
