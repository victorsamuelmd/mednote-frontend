module Page.Autenticar exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onBlur, onFocus)
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


inicial =
    { usuario = ""
    , palabraClave = ""
    , campoEnfocado = Nothing
    , error = Nothing
    }


type Msg
    = DefinirUsuario String
    | DefinirPalabraClave String
    | CampoEnfocado Campo
    | CampoDesenfocado


update msg model =
    case msg of
        DefinirUsuario str ->
            { model | usuario = str }

        DefinirPalabraClave str ->
            { model | palabraClave = str }

        CampoEnfocado campo ->
            { model | campoEnfocado = Just campo }

        CampoDesenfocado ->
            { model | campoEnfocado = Nothing }


validador =
    Validate.all
        [ Validate.ifBlank .usuario ( UsuarioCampo, "Ingrese por favor un nombre de usuario" )
        , Validate.ifBlank .palabraClave ( PalabraClaveCampo, "Ingrese por favor una contrasena" )
        ]


view : Model -> Html Msg
view model =
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


showError : Maybe String -> Html Msg
showError str =
    case str of
        Just string ->
            span [] [ text string ]

        Nothing ->
            text ""
