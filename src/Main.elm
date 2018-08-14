module Main exposing (..)

import Navigation exposing (Location, newUrl)
import Html exposing (Html, text, div, input, label, button, a, p, span, i)
import Html.Attributes exposing (src, class, type_, value, name, defaultValue)
import Html.Events exposing (onInput, onClick)
import Json.Encode as Encode
import Json.Decode as Decode exposing (field)
import Http
import HttpBuilder
import Ports
import Data.Session exposing (..)
import Request.Usuario as Usuario
import Request.Paciente as Paciente
import Router exposing (..)
import Page.Admin as Admin
import Page.Medico as Medico
import Page.Paciente as Pacientes
import Page.Autenticar as Autenticar
import Page.EditarPaciente as EditarPaciente


---- MODEL ----


type Model
    = Autenticar Autenticar.Model
    | Pacientes Session Pacientes.Model
    | Admin Session Admin.Model
    | Medico Session Medico.Model
    | EditarPaciente Session EditarPaciente.Model
    | NotFound


init : Decode.Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        currentRoute =
            parseLocation location

        maybeSession =
            decodeSessionVal val
    in
        case maybeSession of
            Just session ->
                setPage session currentRoute

            Nothing ->
                ( Autenticar Autenticar.inicial, Cmd.none )


setPage : Session -> Route -> ( Model, Cmd Msg )
setPage session route =
    case route of
        AdminRoute ->
            ( Admin session Admin.inicial
            , Cmd.map AdminMsg
                (Usuario.solicitar
                    session.autorizacion
                    Admin.SolicitarUsuariosHttp
                )
            )

        MedicoRoute ->
            ( Medico session Medico.init, Cmd.none )

        PacientesRoute ->
            ( Pacientes session Pacientes.init, Cmd.none )

        EditarPacienteRoute id ->
            ( EditarPaciente session EditarPaciente.init
            , Cmd.map EditarPacienteMsg
                (Paciente.obtener session id EditarPaciente.ResultadoObtenerPaciente)
            )

        LoginRoute ->
            ( Autenticar Autenticar.inicial, Cmd.none )

        NotFoundRoute ->
            ( NotFound, Cmd.none )


decodeSessionVal : Decode.Value -> Maybe Session
decodeSessionVal val =
    Decode.decodeValue decodeSession val
        |> Result.toMaybe


type Msg
    = OnLocationChange Location
    | AdminMsg Admin.Msg
    | AutenticarMsg Autenticar.Msg
    | PacientesMsg Pacientes.Msg
    | MedicoMsg Medico.Msg
    | EditarPacienteMsg EditarPaciente.Msg
    | EnviarAutenticar
    | EnviarAutenticarHttp (Result Http.Error Session)
    | Salir



---- UPDATE ----


enviarAutenticar : { a | palabraClave : String, usuario : String } -> Cmd Msg
enviarAutenticar cred =
    HttpBuilder.post "http://localhost:8070/login"
        |> HttpBuilder.withJsonBody (credEncoder cred)
        |> HttpBuilder.withExpect (Http.expectJson decodeSession)
        |> HttpBuilder.send EnviarAutenticarHttp


credEncoder : { a | palabraClave : String, usuario : String } -> Encode.Value
credEncoder model =
    Encode.object
        [ ( "username", Encode.string model.usuario )
        , ( "password", Encode.string model.palabraClave )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( OnLocationChange location, Pacientes session _ ) ->
            parseLocation location
                |> setPage session

        ( OnLocationChange location, Admin session _ ) ->
            parseLocation location
                |> setPage session

        ( OnLocationChange location, Medico session _ ) ->
            parseLocation location
                |> setPage session

        ( OnLocationChange location, EditarPaciente session _ ) ->
            parseLocation location
                |> setPage session

        ( OnLocationChange location, _ ) ->
            ( NotFound, Navigation.load location.href ) |> Debug.log location.href

        ( AutenticarMsg msg, Autenticar autenticarModel ) ->
            let
                newModel =
                    Autenticar.update msg autenticarModel
            in
                ( Autenticar newModel, Cmd.none )

        ( EnviarAutenticar, Autenticar autenticarModel ) ->
            ( Autenticar autenticarModel, enviarAutenticar autenticarModel )

        ( EnviarAutenticarHttp (Ok session), _ ) ->
            ( Pacientes session Pacientes.init, Ports.storeSession session.autorizacion )

        ( EnviarAutenticarHttp (Err err), Autenticar autenticarModel ) ->
            ( Autenticar { autenticarModel | error = Just "Error" }, Cmd.none )

        ( PacientesMsg msg, Pacientes session pacientesModel ) ->
            let
                ( newModel, subCmd ) =
                    Pacientes.update session msg pacientesModel
            in
                ( Pacientes session newModel, Cmd.map PacientesMsg subCmd )

        ( AdminMsg msg, Admin session adminModel ) ->
            let
                ( newModel, subCmd ) =
                    Admin.update session msg adminModel
            in
                ( Admin session newModel, Cmd.map AdminMsg subCmd )

        ( MedicoMsg msg, Medico session medicoModel ) ->
            let
                ( newModel, subCmd ) =
                    Medico.update session msg medicoModel
            in
                ( Medico session newModel, Cmd.map MedicoMsg subCmd )

        ( EditarPacienteMsg msg, EditarPaciente session medicoModel ) ->
            let
                ( newModel, subCmd ) =
                    EditarPaciente.update session msg medicoModel
            in
                ( EditarPaciente session newModel, Cmd.map EditarPacienteMsg subCmd )

        ( Salir, _ ) ->
            ( Autenticar Autenticar.inicial, Cmd.batch [ newUrl "/", Ports.toJs "salir" ] )

        ( _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


navBar : String -> Html Msg
navBar usuario =
    div [ class "navbar" ]
        [ div [ class "container" ]
            [ div [ class "navbar-brand" ]
                [ span [ class "navbar-item" ] [ text "MedNote" ] ]
            , div [ class "navbar-menu" ]
                [ div [ class "navbar-start" ]
                    []
                , div [ class "navbar-end" ]
                    [ a [ class "navbar-item", onClick Salir ]
                        [ span [ class "icon" ] [ i [ class "fas fa-sign-out-alt" ] [] ], text "Salir" ]
                    , p [ class "navbar-item" ]
                        [ span [ class "icon" ] [ i [ class "fas fa-user" ] [] ], text usuario ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Autenticar modelAutenticar ->
            div [ class "hero is-success is-fullheight" ]
                [ div [ class "hero-body" ]
                    [ div [ class "container has-text-centered" ]
                        [ Html.h3 [ class "title has-text-gray" ] [ text "MedNote" ]
                        , div [ class "column is-4 is-offset-4" ]
                            [ div [ class "box" ]
                                [ Autenticar.view modelAutenticar |> Html.map AutenticarMsg
                                , Html.a [ class "button is-primary is-large is-fullwidth is-block", onClick EnviarAutenticar ] [ text "Autenticar" ]
                                ]
                            ]
                        ]
                    ]
                ]

        Pacientes session modelPacientes ->
            div []
                [ navBar session.usuario
                , Pacientes.view modelPacientes
                    |> Html.map PacientesMsg
                ]

        Admin session modelAdmin ->
            div []
                [ navBar session.usuario
                , Admin.view modelAdmin
                    |> Html.map AdminMsg
                ]

        Medico session modelMedico ->
            div []
                [ navBar session.usuario
                , Medico.view modelMedico
                    |> Html.map MedicoMsg
                ]

        EditarPaciente session modelEditar ->
            div []
                [ navBar session.usuario
                , EditarPaciente.view modelEditar
                    |> Html.map EditarPacienteMsg
                ]

        NotFound ->
            div [] [ text "Not Found" ]


main : Program Decode.Value Model Msg
main =
    Navigation.programWithFlags OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
