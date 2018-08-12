module Main exposing (..)


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


crearPaciente : Model -> Cmd Msg
crearPaciente { autorizacion, perfil } =
    let
        server =
            "http://localhost:8070/perfiles"
    in
        Http.send EnviarCrearPacienteHttp <|
            Http.request
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" (Maybe.withDefault "" autorizacion)
                    , Http.header "Content-Type" "application/json"
                    ]
                , url = server
                , body = encodePaciente perfil |> Http.jsonBody
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
