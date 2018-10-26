module Page.Autenticar exposing (Model, Msg, init, update, view)

import Data.Session exposing (decodeSession)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Http
import HttpBuilder
import Json.Encode as Encode
import Ports
import Validate exposing (validate)


type Campo
    = UsuarioCampo
    | PalabraClaveCampo


type alias Model =
    { usuario : String
    , palabraClave : String
    , campoEnfocado : Maybe Campo
    , error : Maybe String
    }


init =
    { usuario = ""
    , palabraClave = ""
    , campoEnfocado = Nothing
    , error = Nothing
    }


type Msg
    = DefinirUsuario String
    | DefinirPalabraClave String
    | EnviarAutenticar
    | EnviarAutenticarHttp (Result Http.Error Data.Session.Session)
    | CampoEnfocado Campo
    | CampoDesenfocado


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DefinirUsuario str ->
            ( { model | usuario = str }, Cmd.none )

        DefinirPalabraClave str ->
            ( { model | palabraClave = str }, Cmd.none )

        CampoEnfocado campo ->
            ( { model | campoEnfocado = Just campo }, Cmd.none )

        CampoDesenfocado ->
            ( { model | campoEnfocado = Nothing }, Cmd.none )

        EnviarAutenticar ->
            ( model, enviarAutenticar model )

        EnviarAutenticarHttp (Ok session) ->
            ( { model | error = Nothing }, Ports.storeSession session.autorizacion )

        EnviarAutenticarHttp (Err err) ->
            case err of
                Http.BadUrl str ->
                    ( { model | error = Just "No debe ocurrir" }, Cmd.none )

                Http.Timeout ->
                    ( { model | error = Just "No fue posible la conexion" }, Cmd.none )

                Http.NetworkError ->
                    ( { model | error = Just "No fue posible la conexion" }, Cmd.none )

                Http.BadStatus _ ->
                    ( { model | error = Just "Usuario o contrasena incorrectos" }, Cmd.none )

                Http.BadPayload str _ ->
                    ( { model | error = Just str }, Cmd.none )


validador =
    Validate.all
        [ Validate.ifBlank .usuario ( UsuarioCampo, "Ingrese por favor un nombre de usuario" )
        , Validate.ifBlank .palabraClave ( PalabraClaveCampo, "Ingrese por favor una contrasena" )
        ]


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ div [ class "field" ]
            [ input
                [ placeholder "Usuario"
                , onInput DefinirUsuario
                , type_ "text"
                , class "input"
                , onFocus <| CampoEnfocado UsuarioCampo
                , onBlur CampoDesenfocado
                ]
                []
            ]
        , div [ class "field" ]
            [ input
                [ placeholder "Contrasena"
                , onInput DefinirPalabraClave
                , type_ "password"
                , class "input"
                , onFocus <| CampoEnfocado PalabraClaveCampo
                , onBlur CampoDesenfocado
                ]
                []
            ]
        , showError model.error
        ]


view model =
    div [ class "hero is-success is-fullheight" ]
        [ div [ class "hero-body" ]
            [ div [ class "container has-text-centered" ]
                [ Html.h3 [ class "title has-text-gray" ] [ text "MedNote" ]
                , div [ class "column is-4 is-offset-4" ]
                    [ div [ class "box" ]
                        [ viewForm model
                        , Html.button [ class "button is-primary is-large is-fullwidth is-block", onClick EnviarAutenticar ] [ text "Autenticar" ]
                        ]
                    ]
                ]
            ]
        ]



-- Internals


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


showError : Maybe String -> Html Msg
showError str =
    case str of
        Just string ->
            span [] [ text string ]

        Nothing ->
            text ""
