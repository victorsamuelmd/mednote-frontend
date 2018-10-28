module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Page.Admin as Admin
import Page.Autenticar as Autenticar
import Page.Medico as Medico
import Page.Paciente as Paciente
import Ports
import Request.Usuario as Usuario
import Router exposing (..)
import Url



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Model
    = Autenticar Nav.Key Autenticar.Model
    | Admin Nav.Key Session Admin.Model
    | Medico Nav.Key Session Medico.Model
    | Paciente Nav.Key Session Paciente.Model
    | NotFound Nav.Key Session


getKey : Model -> Nav.Key
getKey model =
    case model of
        Autenticar keyNav _ ->
            keyNav

        Admin keyNav _ _ ->
            keyNav

        Medico keyNav _ _ ->
            keyNav

        Paciente keyNav _ _ ->
            keyNav

        NotFound keyNav _ ->
            keyNav


toSession : Model -> Maybe Session
toSession model =
    case model of
        Autenticar keyNav modelAutenticar ->
            Nothing

        Admin _ session _ ->
            Just session

        Medico _ session _ ->
            Just session

        Paciente _ session _ ->
            Just session

        NotFound _ session ->
            Just session


setPage : Nav.Key -> Session -> Route -> ( Model, Cmd Msg )
setPage key session route =
    case route of
        AdminRoute ->
            Admin.init session |> setWith (Admin key session) (Cmd.map AdminMsg)

        MedicoRoute ->
            ( Medico.init, Cmd.none ) |> setWith (Medico key session) (Cmd.map MedicoMsg)

        LoginRoute ->
            ( Autenticar key Autenticar.init, Cmd.none )

        PacientesRoute ->
            ( Paciente.init, Cmd.none ) |> setWith (Paciente key session) (Cmd.map PacienteMsg)

        _ ->
            ( NotFound key session, Cmd.none )


setWith a b c =
    c |> Tuple.mapFirst a |> Tuple.mapSecond b


manageUrl : Url.Url -> Model -> ( Model, Cmd Msg )
manageUrl url model =
    case toSession model of
        Just session ->
            setPage (getKey model) session (parseLocation url)

        Nothing ->
            ( Autenticar (getKey model) Autenticar.init, Cmd.none )


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        currentRoute =
            parseLocation url

        maybeSession =
            decodeSessionVal flags
    in
    case maybeSession of
        Just session ->
            setPage key session currentRoute

        Nothing ->
            ( Autenticar key Autenticar.init, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AutenticarMsg Autenticar.Msg
    | AdminMsg Admin.Msg
    | MedicoMsg Medico.Msg
    | PacienteMsg Paciente.Msg
    | GotSession Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, Autenticar key _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( LinkClicked urlRequest, Admin key _ _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( AutenticarMsg subMsg, Autenticar key subModel ) ->
            Autenticar.update subMsg subModel
                |> setWith (Autenticar key) (Cmd.map AutenticarMsg)

        ( AdminMsg subMsg, Admin key session subModel ) ->
            Admin.update session subMsg subModel
                |> setWith (Admin key session) (Cmd.map AdminMsg)

        ( MedicoMsg subMsg, Medico key session subModel ) ->
            Medico.update session subMsg subModel
                |> setWith (Medico key session) (Cmd.map MedicoMsg)

        ( PacienteMsg subMsg, Paciente key session subModel ) ->
            Paciente.update session subMsg subModel
                |> setWith (Paciente key session) (Cmd.map PacienteMsg)

        ( UrlChanged url, _ ) ->
            manageUrl url model

        ( GotSession val, Autenticar key _ ) ->
            case Decode.decodeValue Session.decodeSession val of
                Ok session ->
                    Admin.init session
                        |> Tuple.mapFirst (Admin key session)
                        |> Tuple.mapSecond (\a -> Cmd.batch [ Nav.pushUrl key "/admin", Cmd.map AdminMsg a ])

                Err str ->
                    ( model, Cmd.none ) |> Debug.log (Debug.toString str)

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Autenticar keyNav modelAutenticar ->
            Ports.gotSession GotSession

        Admin keyNav session modelAdmin ->
            Sub.none

        Medico keyNav session modelAdmin ->
            Sub.none

        Paciente keyNav session modelAdmin ->
            Sub.none

        NotFound _ _ ->
            Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        Autenticar key record ->
            { title = "MedNote | Ingreso al Sistema"
            , body = [ Autenticar.view record |> Html.map AutenticarMsg ]
            }

        Admin key session record ->
            { title = "MedNote | Administrador"
            , body = [ Admin.view record |> Html.map AdminMsg ]
            }

        Medico key session record ->
            { title = "MedNote | Medico"
            , body = [ Medico.view record |> Html.map MedicoMsg ]
            }

        Paciente key session record ->
            { title = "MedNote | Paciente"
            , body = [ Paciente.view record |> Html.map PacienteMsg ]
            }

        NotFound _ _ ->
            { title = "MedNote | Ruta no encontrada"
            , body = []
            }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


decodeSessionVal : Decode.Value -> Maybe Session
decodeSessionVal val =
    Decode.decodeValue Session.decodeSession val
        |> Result.toMaybe



{-
   MedicoRoute ->
       ( Medico session Medico.init, Cmd.none )

   PacientesRoute ->
       ( Pacientes session Pacientes.init, Cmd.none )

   EditarPacienteRoute id ->
       ( EditarPaciente session EditarPaciente.init
       , Cmd.map EditarPacienteMsg
           (Paciente.obtener session id EditarPaciente.ResultadoObtenerPaciente)
       )

   UrgenciasRoute id ->
       ( Urgencias session Urgencias.init, Cmd.none )

-}
{-
   module Main exposing (main)

   import Data.Session exposing (..)
   import Html exposing (Html, a, button, div, i, input, label, p, span, text)
   import Html.Attributes exposing (class, defaultValue, name, src, type_, value)
   import Html.Events exposing (onClick, onInput)
   import Http
   import HttpBuilder
   import Json.Decode as Decode exposing (field)
   import Json.Encode as Encode
   import Page.Admin as Admin
   import Page.Autenticar as Autenticar
   import Page.EditarPaciente as EditarPaciente
   import Page.HistoriaUrgencias as Urgencias
   import Page.Medico as Medico
   import Page.Paciente as Pacientes
   import Ports
   import Request.Paciente as Paciente
   import Router exposing (..)



   ---- MODEL ----


   type Model
       = Autenticar Autenticar.Model
       | Pacientes Session Pacientes.Model
       | Medico Session Medico.Model
       | EditarPaciente Session EditarPaciente.Model
       | Urgencias Session Urgencias.Model


   init : Decode.Value -> Location -> ( Model, Cmd Msg )
   init val location =


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

           UrgenciasRoute id ->
               ( Urgencias session Urgencias.init, Cmd.none )

           LoginRoute ->
               ( Autenticar Autenticar.inicial, Cmd.none )

           NotFoundRoute ->
               ( NotFound, Cmd.none )




   type Msg
       = OnLocationChange Location
       | AdminMsg Admin.Msg
       | AutenticarMsg Autenticar.Msg
       | PacientesMsg Pacientes.Msg
       | MedicoMsg Medico.Msg
       | EditarPacienteMsg EditarPaciente.Msg
       | UrgenciasMsg Urgencias.Msg
       | EnviarAutenticar
       | EnviarAutenticarHttp (Result Http.Error Session)
       | Salir



   ---- UPDATE ----




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

           ( OnLocationChange location, Urgencias session _ ) ->
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
               Pacientes.update session msg pacientesModel
                   |> Tuple.mapFirst (Pacientes session)
                   |> Tuple.mapSecond (Cmd.map PacientesMsg)

           ( AdminMsg msg, Admin session adminModel ) ->
               Admin.update session msg adminModel
                   |> Tuple.mapFirst (Admin session)
                   |> Tuple.mapSecond (Cmd.map AdminMsg)

           ( MedicoMsg msg, Medico session medicoModel ) ->
               Medico.update session msg medicoModel
                   |> Tuple.mapFirst (Medico session)
                   |> Tuple.mapSecond (Cmd.map MedicoMsg)

           ( EditarPacienteMsg msg, EditarPaciente session medicoModel ) ->
               EditarPaciente.update session msg medicoModel
                   |> Tuple.mapFirst (EditarPaciente session)
                   |> Tuple.mapSecond (Cmd.map EditarPacienteMsg)

           ( UrgenciasMsg msg, Urgencias session medicoModel ) ->
               Urgencias.update session msg medicoModel
                   |> Tuple.mapFirst (Urgencias session)
                   |> Tuple.mapSecond (Cmd.map UrgenciasMsg)

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

           Urgencias session modelUrgencias ->
               div []
                   [ navBar session.usuario
                   , Urgencias.view modelUrgencias
                       |> Html.map UrgenciasMsg
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



-}
