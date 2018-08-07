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
    , errores : List ( Campo, String )
    }


inicial =
    { usuario = ""
    , palabraClave = ""
    , campoEnfocado = Nothing
    , errores = []
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


view model =
    div []
        [ input [ placeholder "Usuario", onInput DefinirUsuario ] []
        , input [ placeholder "Contrasena", onInput DefinirPalabraClave ] []
        ]
