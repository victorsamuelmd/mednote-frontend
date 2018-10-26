module Helpers exposing (inputControl, textareaControl)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as JsDecode


inputControl : String -> String -> String -> Maybe String -> (String -> msg) -> Html msg
inputControl labelText nameText valueText err msg =
    Html.div [ Attributes.class "field" ]
        [ Html.label [ Attributes.class "label" ] [ Html.text labelText ]
        , Html.div [ Attributes.class "control" ]
            [ Html.input
                [ Attributes.type_ "text"
                , Attributes.class "input"
                , Attributes.name nameText
                , Attributes.id nameText
                , Attributes.value valueText
                , Events.onInput msg
                ]
                []
            ]
        , case err of
            Just error ->
                Html.p [ Attributes.class "help is-danger" ] [ Html.text error ]

            Nothing ->
                Html.text ""
        ]


textareaControl : String -> String -> String -> (String -> msg) -> Html msg
textareaControl labelText nameText valueText msg =
    Html.div [ Attributes.class "form-group" ]
        [ Html.label [] [ Html.text labelText ]
        , Html.textarea
            [ Attributes.class "form-control"
            , Attributes.name nameText
            , Attributes.id nameText
            , Attributes.value valueText
            , Events.onInput msg
            ]
            []
        ]
