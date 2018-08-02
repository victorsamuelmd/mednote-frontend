module Page.HistoriaUrgencias exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Data.HistoriaUrgencias exposing (..)
import Validate exposing (Validator, ifBlank, ifNotInt, validate)


type Msg
    = DefinirAdmitidoPor String
    | DefinirRegistro String
    | DefinirOrigenAtencion String
    | DefinirMotivoConsulta String
    | DefinirEnfermedadActual String
    | DefinirAntecedentesFamiliares String
    | DefinirAntecedentesPersonales String
    | DefinirRevisionSistemas String
    | DefinirRespuestaOcular String
    | DefinirRespuestaVerbal String
    | DefinirRespuestaMotora String
    | DefinirFuerzaSuperiorI String
    | DefinirFuerzaSuperiorD String
    | DefinirFuerzaInferiorI String
    | DefinirFuerzaInferiorD String
    | DefinirReflejosSuperiorI String
    | DefinirReflejosSuperiorD String
    | DefinirReflejosInferiorI String
    | DefinirReflejosInferiorD String
    | DefinirEstadoGeneral String
    | DefinirTensionArterialSistolica String
    | DefinirTensionArterialDiastolica String
    | DefinirFrecuenciaCardiaca String
    | DefinirFrecuenciaRespiratoria String
    | DefinirTemperatura String
    | DefinirSaturacionOxigeno String
    | DefinirPeso String
    | DefinirTalla String
    | DefinirExamenFisico String
    | DefinirAnalisisConducta String
    | DefinirDiagnosticoPrincipal String
    | DefinirDiagnosticoRelacionado1 String
    | DefinirDiagnosticoRelacionado2 String
    | DefinirDiagnosticoRelacionado3 String


type RespuestaOcular
    = Espontanea
    | AlLlamado
    | AlDolor
    | NoApertura


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DefinirAdmitidoPor a ->
            { model | admitidoPor = a } ! []

        DefinirRegistro a ->
            { model | registro = a } ! []

        DefinirOrigenAtencion a ->
            { model | origenAtencion = a } ! []

        DefinirMotivoConsulta a ->
            { model | motivoConsulta = a } ! []

        DefinirEnfermedadActual a ->
            { model | enfermedadActual = a } ! []

        DefinirAntecedentesFamiliares a ->
            { model | antecedentesFamiliares = a } ! []

        DefinirAntecedentesPersonales a ->
            { model | antecedentesPersonales = a } ! []

        DefinirRevisionSistemas a ->
            { model | revisionSistemas = a } ! []

        DefinirRespuestaOcular a ->
            { model | respuestaOcular = Result.withDefault 0 <| String.toInt a } ! []

        DefinirRespuestaVerbal a ->
            { model | respuestaVerbal = Result.withDefault 0 <| String.toInt a } ! []

        DefinirRespuestaMotora a ->
            { model | respuestaMotora = Result.withDefault 0 <| String.toInt a } ! []

        DefinirFuerzaSuperiorI a ->
            { model | fuerzaSuperiorI = Result.withDefault 0 <| String.toInt a } ! []

        DefinirFuerzaSuperiorD a ->
            { model | fuerzaSuperiorD = Result.withDefault 0 <| String.toInt a } ! []

        DefinirFuerzaInferiorI a ->
            { model | fuerzaInferiorI = Result.withDefault 0 <| String.toInt a } ! []

        DefinirFuerzaInferiorD a ->
            { model | fuerzaInferiorD = Result.withDefault 0 <| String.toInt a } ! []

        DefinirReflejosSuperiorI a ->
            { model | reflejosSuperiorI = a } ! []

        DefinirReflejosSuperiorD a ->
            { model | reflejosSuperiorD = a } ! []

        DefinirReflejosInferiorI a ->
            { model | reflejosInferiorI = a } ! []

        DefinirReflejosInferiorD a ->
            { model | reflejosInferiorD = a } ! []

        DefinirEstadoGeneral a ->
            { model | estadoGeneral = a } ! []

        DefinirTensionArterialSistolica a ->
            { model | tensionArterialSistolica = Result.withDefault 0 <| String.toInt a } ! []

        DefinirTensionArterialDiastolica a ->
            { model | tensionArterialDiastolica = Result.withDefault 0 <| String.toInt a } ! []

        DefinirFrecuenciaCardiaca a ->
            { model | frecuenciaCardiaca = Result.withDefault 0 <| String.toInt a } ! []

        DefinirFrecuenciaRespiratoria a ->
            { model | frecuenciaRespiratoria = Result.withDefault 0 <| String.toInt a } ! []

        DefinirTemperatura a ->
            { model | temperatura = Result.withDefault 0.0 <| String.toFloat a } ! []

        DefinirSaturacionOxigeno a ->
            { model | saturacionOxigeno = Result.withDefault 0 <| String.toInt a } ! []

        DefinirPeso a ->
            { model | peso = Result.withDefault 0.0 <| String.toFloat a } ! []

        DefinirTalla a ->
            { model | talla = Result.withDefault 0.0 <| String.toFloat a } ! []

        DefinirExamenFisico a ->
            { model | examenFisico = a } ! []

        DefinirAnalisisConducta a ->
            { model | analisisConducta = a } ! []

        DefinirDiagnosticoPrincipal a ->
            { model | diagnosticoPrincipal = a } ! []

        DefinirDiagnosticoRelacionado1 a ->
            { model | diagnosticoRelacionado1 = a } ! []

        DefinirDiagnosticoRelacionado2 a ->
            { model | diagnosticoRelacionado2 = a } ! []

        DefinirDiagnosticoRelacionado3 a ->
            { model | diagnosticoRelacionado3 = a } ! []


inputControl : String -> String -> String -> (String -> msg) -> Html msg
inputControl labelText nameText valueText msg =
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , input
            [ type_ "text"
            , class "form-control"
            , name nameText
            , id nameText
            , defaultValue valueText
            , onInput msg
            ]
            []
        ]


textareaControl : String -> String -> String -> (String -> msg) -> Html msg
textareaControl labelText nameText valueText msg =
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , textarea
            [ class "form-control"
            , name nameText
            , id nameText
            , defaultValue valueText
            , onInput msg
            ]
            []
        ]


view : Model -> Html Msg
view model =
    div []
        [ inputControl "Admitido Por" "admitidoPor" model.admitidoPor DefinirAdmitidoPor
        , inputControl "Registro" "registro" model.registro DefinirRegistro
        , inputControl "Origen Atencion" "origenAtencion" model.origenAtencion DefinirOrigenAtencion
        , inputControl "Motivo Consulta" "motivoConsulta" model.motivoConsulta DefinirMotivoConsulta
        , inputControl "Enfermedad Actual" "enfermedadActual" model.enfermedadActual DefinirEnfermedadActual
        , inputControl "Antecedentes Familiares" "antecedentesFamiliares" model.antecedentesFamiliares DefinirAntecedentesFamiliares
        , inputControl "Antecedentes Personales" "antecedentesPersonales" model.antecedentesPersonales DefinirAntecedentesPersonales
        , inputControl "Revision Sistemas" "revisionSistemas" model.revisionSistemas DefinirRevisionSistemas
        , inputControl "Respuesta Ocular" "respuestaOcular" (toString model.respuestaOcular) DefinirRespuestaOcular
        , inputControl "Respuesta Verbal" "respuestaVerbal" (toString model.respuestaVerbal) DefinirRespuestaVerbal
        , inputControl "Respuesta Motora" "respuestaMotora" (toString model.respuestaMotora) DefinirRespuestaMotora
        , inputControl "Fuerza Superior I" "fuerzaSuperiorI" (toString model.fuerzaSuperiorI) DefinirFuerzaSuperiorI
        , inputControl "Fuerza Superior D" "fuerzaSuperiorD" (toString model.fuerzaSuperiorD) DefinirFuerzaSuperiorD
        , inputControl "Fuerza Inferior I" "fuerzaInferiorI" (toString model.fuerzaInferiorI) DefinirFuerzaInferiorI
        , inputControl "Fuerza Inferior D" "fuerzaInferiorD" (toString model.fuerzaInferiorD) DefinirFuerzaInferiorD
        , inputControl "Reflejos Superior I" "reflejosSuperiorI" model.reflejosSuperiorI DefinirReflejosSuperiorI
        , inputControl "Reflejos Superior D" "reflejosSuperiorD" model.reflejosSuperiorD DefinirReflejosSuperiorD
        , inputControl "Reflejos Inferior I" "reflejosInferiorI" model.reflejosInferiorI DefinirReflejosInferiorI
        , inputControl "Reflejos Inferior D" "reflejosInferiorD" model.reflejosInferiorD DefinirReflejosInferiorD
        , inputControl "Estado General" "estadoGeneral" model.estadoGeneral DefinirEstadoGeneral
        , inputControl "Tension Arterial Sistolica" "tensionArterialSistolica" (toString model.tensionArterialSistolica) DefinirTensionArterialSistolica
        , inputControl "Tension Arterial Diastolica" "tensionArterialDiastolica" (toString model.tensionArterialDiastolica) DefinirTensionArterialDiastolica
        , inputControl "Frecuencia Cardiaca" "frecuenciaCardiaca" (toString model.frecuenciaCardiaca) DefinirFrecuenciaCardiaca
        , inputControl "Frecuencia Respiratoria" "frecuenciaRespiratoria" (toString model.frecuenciaRespiratoria) DefinirFrecuenciaRespiratoria
        , inputControl "Temperatura" "temperatura" (toString model.temperatura) DefinirTemperatura
        , inputControl "Saturacion Oxigeno" "saturacionOxigeno" (toString model.saturacionOxigeno) DefinirSaturacionOxigeno
        , inputControl "Peso" "peso" (toString model.peso) DefinirPeso
        , inputControl "Talla" "talla" (toString model.talla) DefinirTalla
        , inputControl "Examen Fisico" "examenFisico" model.examenFisico DefinirExamenFisico
        , inputControl "Analisis Conducta" "analisisConducta" model.analisisConducta DefinirAnalisisConducta
        , inputControl "Diagnostico Principal" "diagnosticoPrincipal" model.diagnosticoPrincipal DefinirDiagnosticoPrincipal
        , inputControl "Diagnostico Relacionado 1" "diagnosticoRelacionado1" model.diagnosticoRelacionado1 DefinirDiagnosticoRelacionado1
        , inputControl "Diagnostico Relacionado 2" "diagnosticoRelacionado2" model.diagnosticoRelacionado2 DefinirDiagnosticoRelacionado2
        , inputControl "Diagnostico Relacionado 3" "diagnosticoRelacionado3" model.diagnosticoRelacionado3 DefinirDiagnosticoRelacionado3
        ]


modelValidator =
    Validate.all
        [ ifBlank .admitidoPor "Este campo es obligatorio"
        , ifBlank .registro "Este campo es obligatorio"
        , ifBlank .origenAtencion "Este campo es obligatorio"
        , ifBlank .motivoConsulta "Este campo es obligatorio"
        , ifBlank .enfermedadActual "Este campo es obligatorio"
        , ifBlank .antecedentesFamiliares "Este campo es obligatorio"
        , ifBlank .antecedentesPersonales "Este campo es obligatorio"
        , ifBlank .revisionSistemas "Este campo es obligatorio"
        , ifBlank .reflejosSuperiorI "Este campo es obligatorio"
        , ifBlank .reflejosSuperiorD "Este campo es obligatorio"
        , ifBlank .reflejosInferiorI "Este campo es obligatorio"
        , ifBlank .reflejosInferiorD "Este campo es obligatorio"
        , ifBlank .estadoGeneral "Este campo es obligatorio"
        , ifBlank .examenFisico "Este campo es obligatorio"
        , ifBlank .analisisConducta "Este campo es obligatorio"
        , ifBlank .diagnosticoPrincipal "Este campo es obligatorio"
        , ifBlank .diagnosticoRelacionado1 "Este campo es obligatorio"
        , ifBlank .diagnosticoRelacionado2 "Este campo es obligatorio"
        , ifBlank .diagnosticoRelacionado3 "Este campo es obligatorio"
        ]
