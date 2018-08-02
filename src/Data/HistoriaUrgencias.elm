module Data.HistoriaUrgencias exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type alias Model =
    { id : String
    , paciente : String
    , fechaInicio : String
    , fechaFinalizacion : String
    , admitidoPor : String
    , registro : String
    , origenAtencion : String
    , motivoConsulta : String
    , enfermedadActual : String
    , antecedentesFamiliares : String
    , antecedentesPersonales : String
    , revisionSistemas : String
    , respuestaOcular : Int
    , respuestaVerbal : Int
    , respuestaMotora : Int
    , fuerzaSuperiorI : Int
    , fuerzaSuperiorD : Int
    , fuerzaInferiorI : Int
    , fuerzaInferiorD : Int
    , reflejosSuperiorI : String
    , reflejosSuperiorD : String
    , reflejosInferiorI : String
    , reflejosInferiorD : String
    , estadoGeneral : String
    , tensionArterialSistolica : Int
    , tensionArterialDiastolica : Int
    , frecuenciaCardiaca : Int
    , frecuenciaRespiratoria : Int
    , temperatura : Float
    , saturacionOxigeno : Int
    , peso : Float
    , talla : Float
    , examenFisico : String
    , analisisConducta : String
    , diagnosticoPrincipal : String
    , diagnosticoRelacionado1 : String
    , diagnosticoRelacionado2 : String
    , diagnosticoRelacionado3 : String
    , medico : String
    }


initialHistoriaUrgencias : Model
initialHistoriaUrgencias =
    { id = ""
    , paciente = ""
    , fechaInicio = ""
    , fechaFinalizacion = ""
    , admitidoPor = ""
    , registro = ""
    , origenAtencion = ""
    , motivoConsulta = ""
    , enfermedadActual = ""
    , antecedentesFamiliares = ""
    , antecedentesPersonales = ""
    , revisionSistemas = ""
    , respuestaOcular = 4
    , respuestaVerbal = 5
    , respuestaMotora = 6
    , fuerzaSuperiorI = 4
    , fuerzaSuperiorD = 4
    , fuerzaInferiorI = 4
    , fuerzaInferiorD = 4
    , reflejosSuperiorI = ""
    , reflejosSuperiorD = ""
    , reflejosInferiorI = ""
    , reflejosInferiorD = ""
    , estadoGeneral = ""
    , tensionArterialSistolica = 0
    , tensionArterialDiastolica = 0
    , frecuenciaCardiaca = 0
    , frecuenciaRespiratoria = 0
    , temperatura = 0.0
    , saturacionOxigeno = 0
    , peso = 0.0
    , talla = 0.0
    , examenFisico = ""
    , analisisConducta = ""
    , diagnosticoPrincipal = ""
    , diagnosticoRelacionado1 = ""
    , diagnosticoRelacionado2 = ""
    , diagnosticoRelacionado3 = ""
    , medico = ""
    }


decodeHistoriaUrgencias : Decode.Decoder Model
decodeHistoriaUrgencias =
    decode Model
        |> required "id" Decode.string
        |> required "paciente" Decode.string
        |> required "fechaInicio" Decode.string
        |> required "fechaFinalizacion" Decode.string
        |> required "admitidoPor" Decode.string
        |> required "registro" Decode.string
        |> required "origenAtencion" Decode.string
        |> required "motivoConsulta" Decode.string
        |> required "enfermedadActual" Decode.string
        |> required "antecedentesFamiliares" Decode.string
        |> required "antecedentesPersonales" Decode.string
        |> required "revisionSistemas" Decode.string
        |> required "respuestaOcular" Decode.int
        |> required "respuestaVerbal" Decode.int
        |> required "respuestaMotora" Decode.int
        |> required "fuerzaSuperiorI" Decode.int
        |> required "fuerzaSuperiorD" Decode.int
        |> required "fuerzaInferiorI" Decode.int
        |> required "fuerzaInferiorD" Decode.int
        |> required "reflejosSuperiorI" Decode.string
        |> required "reflejosSuperiorD" Decode.string
        |> required "reflejosInferiorI" Decode.string
        |> required "reflejosInferiorD" Decode.string
        |> required "estadoGeneral" Decode.string
        |> required "tensionArterialSistolica" Decode.int
        |> required "tensionArterialDiastolica" Decode.int
        |> required "frecuenciaCardiaca" Decode.int
        |> required "frecuenciaRespiratoria" Decode.int
        |> required "temperatura" Decode.float
        |> required "saturacionOxigeno" Decode.int
        |> required "peso" Decode.float
        |> required "talla" Decode.float
        |> required "examenFisico" Decode.string
        |> required "analisisConducta" Decode.string
        |> required "diagnosticoPrincipal" Decode.string
        |> required "diagnosticoRelacionado1" Decode.string
        |> required "diagnosticoRelacionado2" Decode.string
        |> required "diagnosticoRelacionado3" Decode.string
        |> required "medico" Decode.string


encodeHistoriaUrgencias : Model -> Encode.Value
encodeHistoriaUrgencias record =
    Encode.object
        [ ( "id", Encode.string <| record.id )
        , ( "paciente", Encode.string <| record.paciente )
        , ( "fechaInicio", Encode.string <| record.fechaInicio )
        , ( "fechaFinalizacion", Encode.string <| record.fechaFinalizacion )
        , ( "admitidoPor", Encode.string <| record.admitidoPor )
        , ( "registro", Encode.string <| record.registro )
        , ( "origenAtencion", Encode.string <| record.origenAtencion )
        , ( "motivoConsulta", Encode.string <| record.motivoConsulta )
        , ( "enfermedadActual", Encode.string <| record.enfermedadActual )
        , ( "antecedentesFamiliares", Encode.string <| record.antecedentesFamiliares )
        , ( "antecedentesPersonales", Encode.string <| record.antecedentesPersonales )
        , ( "revisionSistemas", Encode.string <| record.antecedentesPersonales )
        , ( "respuestaOcular", Encode.int <| record.respuestaOcular )
        , ( "respuestaVerbal", Encode.int <| record.respuestaVerbal )
        , ( "respuestaMotora", Encode.int <| record.respuestaMotora )
        , ( "fuerzaSuperiorI", Encode.int <| record.fuerzaSuperiorI )
        , ( "fuerzaSuperiorD", Encode.int <| record.fuerzaSuperiorD )
        , ( "fuerzaInferiorI", Encode.int <| record.fuerzaInferiorI )
        , ( "fuerzaInferiorD", Encode.int <| record.fuerzaInferiorD )
        , ( "reflejosSuperiorI", Encode.string <| record.reflejosSuperiorI )
        , ( "reflejosSuperiorD", Encode.string <| record.reflejosSuperiorD )
        , ( "reflejosInferiorI", Encode.string <| record.reflejosInferiorI )
        , ( "reflejosInferiorD", Encode.string <| record.reflejosInferiorD )
        , ( "estadoGeneral", Encode.string <| record.estadoGeneral )
        , ( "tensionArterialSistolica", Encode.int <| record.tensionArterialSistolica )
        , ( "tensionArterialDiastolica", Encode.int <| record.tensionArterialDiastolica )
        , ( "frecuenciaCardiaca", Encode.int <| record.frecuenciaCardiaca )
        , ( "frecuenciaRespiratoria", Encode.int <| record.frecuenciaRespiratoria )
        , ( "temperatura", Encode.float <| record.temperatura )
        , ( "saturacionOxigeno", Encode.int <| record.saturacionOxigeno )
        , ( "peso", Encode.float <| record.peso )
        , ( "talla", Encode.float <| record.talla )
        , ( "examenFisico", Encode.string <| record.examenFisico )
        , ( "analisisConducta", Encode.string <| record.analisisConducta )
        , ( "diagnosticoPrincipal", Encode.string <| record.diagnosticoPrincipal )
        , ( "diagnosticoRelacionado1", Encode.string <| record.diagnosticoRelacionado1 )
        , ( "diagnosticoRelacionado2", Encode.string <| record.diagnosticoRelacionado2 )
        , ( "diagnosticoRelacionado3", Encode.string <| record.diagnosticoRelacionado3 )
        , ( "medico", Encode.string <| record.medico )
        ]
