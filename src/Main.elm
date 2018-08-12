module Main exposing (..)

import Navigation exposing (Location, newUrl)
import UrlParser exposing (..)
import Html exposing (Html, text, div, input, label, button, a, p, span, i)
import Html.Attributes exposing (src, class, type_, value, name, defaultValue)
import Html.Events exposing (onInput, onClick)
import Json.Encode as Encode
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Http
import HttpBuilder
import Ports
import Page.HistoriaUrgencias
import Data.HistoriaUrgencias exposing (..)
import Data.Paciente exposing (..)
import Request.HistoriaUrgencias
import Request.Admin
import Validate exposing (validate)
import Page.Autenticar as Autenticar
import Page.Admin as Admin
import Router exposing (..)


---- MODEL ----


type State
    = Admin Admin.Model
    | Autenticar Autenticar.Model
    | NotFound


type alias Model =
    { session : Maybe Admin.Credentials, pageState : State }


init : Decode.Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        currentRoute =
            parseLocation location

        session =
            decodeSession val
    in
        case ( currentRoute, session ) of
            ( LoginRoute, Nothing ) ->
                ( { session = Nothing, pageState = Autenticar Autenticar.inicial }, Cmd.none )

            ( _, Nothing ) ->
                ( { pageState = Autenticar Autenticar.inicial, session = Nothing }
                , Cmd.batch [ newUrl "/" ]
                )

            ( AdminRoute, Just session ) ->
                ( { pageState = Admin Admin.inicial, session = Just session }
                , Cmd.batch
                    [ Cmd.map OriginalMsg <|
                        Request.Admin.solicitarUsuarios
                            session.authorization
                            Admin.SolicitarUsuariosHttp
                    ]
                )

            _ ->
                ( { pageState = Autenticar Autenticar.inicial, session = Nothing }
                , Cmd.batch [ newUrl "/" ]
                )


decodeSession val =
    Decode.decodeValue Admin.decodeCredentials val
        |> Result.toMaybe


type Msg
    = OnLocationChange Location
    | OriginalMsg Admin.Msg
    | AutenticarMsg Autenticar.Msg
    | EnviarAutenticar
    | EnviarAutenticarHttp (Result Http.Error Admin.Credentials)
    | Salir



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.pageState msg model


enviarAutenticar cred =
    HttpBuilder.post "http://localhost:8070/login"
        |> HttpBuilder.withJsonBody (credEncoder cred)
        |> HttpBuilder.withExpect (Http.expectJson Admin.decodeCredentials)
        |> HttpBuilder.send EnviarAutenticarHttp


credEncoder model =
    Encode.object
        [ ( "username", Encode.string model.usuario )
        , ( "password", Encode.string model.palabraClave )
        ]


updatePage : State -> Msg -> Model -> ( Model, Cmd Msg )
updatePage state msg model =
    case ( msg, state ) of
        ( EnviarAutenticar, Autenticar subModel ) ->
            ( model, enviarAutenticar subModel )

        ( EnviarAutenticarHttp (Ok cred), Autenticar subModel ) ->
            ( { model | session = Just cred }
            , Cmd.batch
                [ newUrl "#admin"
                , Ports.storeSession cred.authorization
                ]
            )

        ( EnviarAutenticarHttp (Err err), Autenticar subModel ) ->
            ( { model
                | pageState =
                    Autenticar
                        { subModel | error = Just "Usuario o palabra clave invalida" }
              }
            , Cmd.none
            )

        ( OriginalMsg msg, Admin subModel ) ->
            case model.session of
                Just session ->
                    let
                        ( subM, subCmd ) =
                            Admin.update session msg subModel
                    in
                        ( { model | pageState = Admin subM }, Cmd.map OriginalMsg subCmd )

                Nothing ->
                    ( model, Cmd.none )

        ( AutenticarMsg msg, Autenticar subModel ) ->
            let
                subM =
                    Autenticar.update msg subModel
            in
                ( { model | pageState = Autenticar subM }, Cmd.none )

        ( OnLocationChange location, Admin subModel ) ->
            case model.session of
                Just session ->
                    let
                        loc =
                            parseLocation location

                        inicialOriginal =
                            Admin.inicial
                    in
                        case loc of
                            AdminRoute ->
                                ( { model | pageState = Admin subModel }
                                , Cmd.map OriginalMsg <|
                                    Request.Admin.solicitarUsuarios
                                        session.authorization
                                        Admin.SolicitarUsuariosHttp
                                )

                            _ ->
                                ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( OnLocationChange location, _ ) ->
            let
                loc =
                    parseLocation location
            in
                case loc of
                    AdminRoute ->
                        case model.session of
                            Just session ->
                                ( { model | pageState = Admin Admin.inicial }
                                , Cmd.map OriginalMsg <|
                                    Request.Admin.solicitarUsuarios
                                        session.authorization
                                        Admin.SolicitarUsuariosHttp
                                )

                            Nothing ->
                                ( model, newUrl "/" )

                    LoginRoute ->
                        ( { model | pageState = Autenticar Autenticar.inicial }, Cmd.none )

                    NotFoundRoute ->
                        ( { model | pageState = NotFound }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

        ( Salir, _ ) ->
            ( { model
                | session = Nothing
                , pageState = Autenticar Autenticar.inicial
              }
            , Cmd.batch [ Ports.toJs "salir", newUrl "/" ]
            )

        ( _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


zeroOrInt : String -> Int
zeroOrInt str =
    Result.withDefault 0 <| String.toInt str


oneOrInt : String -> Int
oneOrInt str =
    Result.withDefault 1 <| String.toInt str


zeroOrFloat : String -> Float
zeroOrFloat str =
    Result.withDefault 0.0 <| String.toFloat str


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
    case model.pageState of
        Admin subModel ->
            div []
                [ navBar <| Maybe.withDefault "" (Maybe.map .username model.session)
                , Admin.view subModel
                    |> Html.map OriginalMsg
                ]

        Autenticar subModel ->
            div [ class "hero is-success is-fullheight" ]
                [ div [ class "hero-body" ]
                    [ div [ class "container has-text-centered" ]
                        [ Html.h3 [ class "title has-text-gray" ] [ text "MedNote" ]
                        , div [ class "column is-4 is-offset-4" ]
                            [ div [ class "box" ]
                                [ Autenticar.view subModel |> Html.map AutenticarMsg
                                , Html.a [ class "button is-primary is-large is-fullwidth is-block", onClick EnviarAutenticar ] [ text "Autenticar" ]
                                ]
                            ]
                        ]
                    ]
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
