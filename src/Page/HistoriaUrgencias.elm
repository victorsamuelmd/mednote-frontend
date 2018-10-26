module Page.HistoriaUrgencias exposing (Model, Msg, update, view, init)

import Html as Html exposing (Html)
import Html.Attributes as Attributes
import Data.HistoriaUrgencias exposing (Output, HistoriaUrgencias, encodeHistoriaUrgenciasOutput)
import Form.Value as Value exposing (Value)
import Form exposing (Form)
import Http
import Page.BulmaForm as Bulma


type alias Model =
    { aEditar : Bulma.Model Values, id : String }


type Msg
    = FormChanged (Bulma.Model Values)
    | HistoriaUrgenciasLista Output
    | HistoriaUrgenciasListaHttp (Result Http.Error String)


update : session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    ( model, Cmd.none )


init : Model
init =
    { aEditar =
        { admitidoPor = Value.blank
        , registro = Value.blank
        , origenAtencion = Value.blank
        , motivoConsulta = Value.blank
        , enfermedadActual = Value.blank
        , antecedentesFamiliares = Value.blank
        , antecedentesPersonales = Value.blank
        , revisionSistemas = Value.blank
        , respuestaOcular = Value.blank
        , respuestaVerbal = Value.blank
        , respuestaMotora = Value.blank
        , fuerzaSuperiorI = Value.blank
        , fuerzaSuperiorD = Value.blank
        , fuerzaInferiorI = Value.blank
        , fuerzaInferiorD = Value.blank
        , reflejosSuperiorI = Value.blank
        , reflejosSuperiorD = Value.blank
        , reflejosInferiorI = Value.blank
        , reflejosInferiorD = Value.blank
        , estadoGeneral = Value.blank
        , tensionArterialSistolica = Value.blank
        , tensionArterialDiastolica = Value.blank
        , frecuenciaCardiaca = Value.blank
        , frecuenciaRespiratoria = Value.blank
        , temperatura = Value.blank
        , saturacionOxigeno = Value.blank
        , peso = Value.blank
        , talla = Value.blank
        , examenFisico = Value.blank
        , analisisConducta = Value.blank
        , diagnosticoPrincipal = Value.blank
        , diagnosticoRelacionado1 = Value.blank
        , diagnosticoRelacionado2 = Value.blank
        , diagnosticoRelacionado3 = Value.blank
        }
            |> Bulma.idle
    , id = ""
    }


view : Model -> Html Msg
view { aEditar } =
    Html.div [ Attributes.class "container" ]
        [ Html.h2 [ Attributes.class "title" ] [ Html.text "Historia de Urgencias" ]
        , Bulma.asHtml
            { onChange = FormChanged
            , action = "Guardar Cambios"
            , loading = "Loading..."
            , validation = Bulma.ValidateOnSubmit
            }
            (Form.map HistoriaUrgenciasLista form)
            aEditar
        ]


type alias Values =
    { admitidoPor : Value String
    , registro : Value String
    , origenAtencion : Value String
    , motivoConsulta : Value String
    , enfermedadActual : Value String
    , antecedentesFamiliares : Value String
    , antecedentesPersonales : Value String
    , revisionSistemas : Value String
    , respuestaOcular : Value String
    , respuestaVerbal : Value String
    , respuestaMotora : Value String
    , fuerzaSuperiorI : Value String
    , fuerzaSuperiorD : Value String
    , fuerzaInferiorI : Value String
    , fuerzaInferiorD : Value String
    , reflejosSuperiorI : Value String
    , reflejosSuperiorD : Value String
    , reflejosInferiorI : Value String
    , reflejosInferiorD : Value String
    , estadoGeneral : Value String
    , tensionArterialSistolica : Value String
    , tensionArterialDiastolica : Value String
    , frecuenciaCardiaca : Value String
    , frecuenciaRespiratoria : Value String
    , temperatura : Value Float
    , saturacionOxigeno : Value String
    , peso : Value Float
    , talla : Value Float
    , examenFisico : Value String
    , analisisConducta : Value String
    , diagnosticoPrincipal : Value String
    , diagnosticoRelacionado1 : Value String
    , diagnosticoRelacionado2 : Value String
    , diagnosticoRelacionado3 : Value String
    }


admitidoPorField =
    Form.selectField
        { parser = Ok
        , value = .admitidoPor
        , update = \value values -> { values | admitidoPor = value }
        , attributes =
            { label = "AdmitidoPor"
            , placeholder = "AdmitidoPor"
            , options =
                [ ( "urgencias ", "Urgencias" )
                , ( "consultaExterna ", "Consulta Externa" )
                , ( "remitido ", "Remitido" )
                , ( "cirugia ", "Cirugia" )
                ]
            }
        }


registroField =
    Form.textField
        { parser = Ok
        , value = .registro
        , update = \value values -> { values | registro = value }
        , attributes = { label = "Registro", placeholder = "Registro" }
        }


origenAtencionField =
    Form.selectField
        { parser = Ok
        , value = .origenAtencion
        , update = \value values -> { values | origenAtencion = value }
        , attributes =
            { label = "OrigenAtencion"
            , placeholder = "OrigenAtencion"
            , options =
                [ ( "accidenteLaboral ", "Accidente Laboral" )
                , ( "accidenteTransito ", "Accidente Transito" )
                , ( "enfermedadProfesional ", "Enfermedad Profesional" )
                , ( "otroTipoAccidente ", "Otro Tipo Accidente" )
                , ( "lesionPorAgresion ", "Lesion Por Agresion" )
                , ( "lesionAutoinflingida ", "Lesion Autoinflingida" )
                , ( "enfermedadGeneral ", "Enfermedad General" )
                ]
            }
        }


motivoConsultaField =
    Form.textField
        { parser = Ok
        , value = .motivoConsulta
        , update = \value values -> { values | motivoConsulta = value }
        , attributes = { label = "MotivoConsulta", placeholder = "MotivoConsulta" }
        }


enfermedadActualField =
    Form.textareaField
        { parser = Ok
        , value = .enfermedadActual
        , update = \value values -> { values | enfermedadActual = value }
        , attributes = { label = "EnfermedadActual", placeholder = "EnfermedadActual" }
        }


antecedentesFamiliaresField =
    Form.textField
        { parser = Ok
        , value = .antecedentesFamiliares
        , update = \value values -> { values | antecedentesFamiliares = value }
        , attributes = { label = "AntecedentesFamiliares", placeholder = "AntecedentesFamiliares" }
        }


antecedentesPersonalesField =
    Form.textField
        { parser = Ok
        , value = .antecedentesPersonales
        , update = \value values -> { values | antecedentesPersonales = value }
        , attributes = { label = "AntecedentesPersonales", placeholder = "AntecedentesPersonales" }
        }


revisionSistemasField =
    Form.textField
        { parser = Ok
        , value = .revisionSistemas
        , update = \value values -> { values | revisionSistemas = value }
        , attributes = { label = "RevisionSistemas", placeholder = "RevisionSistemas" }
        }


respuestaOcularField =
    Form.textField
        { parser = String.toInt
        , value = .respuestaOcular
        , update = \value values -> { values | respuestaOcular = value }
        , attributes =
            { label = "RespuestaOcular"
            , placeholder = "RespuestaOcular"
            }
        }


respuestaVerbalField =
    Form.textField
        { parser = String.toInt
        , value = .respuestaVerbal
        , update = \value values -> { values | respuestaVerbal = value }
        , attributes =
            { label = "RespuestaVerbal"
            , placeholder = "RespuestaVerbal"
            }
        }


respuestaMotoraField =
    Form.textField
        { parser = String.toInt
        , value = .respuestaMotora
        , update = \value values -> { values | respuestaMotora = value }
        , attributes =
            { label = "RespuestaMotora"
            , placeholder = "RespuestaMotora"
            }
        }


fuerzaSuperiorIField =
    Form.textField
        { parser = String.toInt
        , value = .fuerzaSuperiorI
        , update = \value values -> { values | fuerzaSuperiorI = value }
        , attributes =
            { label = "FuerzaSuperiorI"
            , placeholder = "FuerzaSuperiorI"
            }
        }


fuerzaSuperiorDField =
    Form.textField
        { parser = String.toInt
        , value = .fuerzaSuperiorD
        , update = \value values -> { values | fuerzaSuperiorD = value }
        , attributes =
            { label = "FuerzaSuperiorD"
            , placeholder = "FuerzaSuperiorD"
            }
        }


fuerzaInferiorIField =
    Form.textField
        { parser = String.toInt
        , value = .fuerzaInferiorI
        , update = \value values -> { values | fuerzaInferiorI = value }
        , attributes =
            { label = "FuerzaInferiorI"
            , placeholder = "FuerzaInferiorI"
            }
        }


fuerzaInferiorDField =
    Form.textField
        { parser = String.toInt
        , value = .fuerzaInferiorD
        , update = \value values -> { values | fuerzaInferiorD = value }
        , attributes =
            { label = "FuerzaInferiorD"
            , placeholder = "FuerzaInferiorD"
            }
        }


reflejosSuperiorIField =
    Form.textField
        { parser = Ok
        , value = .reflejosSuperiorI
        , update = \value values -> { values | reflejosSuperiorI = value }
        , attributes = { label = "ReflejosSuperiorI", placeholder = "ReflejosSuperiorI" }
        }


reflejosSuperiorDField =
    Form.textField
        { parser = Ok
        , value = .reflejosSuperiorD
        , update = \value values -> { values | reflejosSuperiorD = value }
        , attributes = { label = "ReflejosSuperiorD", placeholder = "ReflejosSuperiorD" }
        }


reflejosInferiorIField =
    Form.textField
        { parser = Ok
        , value = .reflejosInferiorI
        , update = \value values -> { values | reflejosInferiorI = value }
        , attributes = { label = "ReflejosInferiorI", placeholder = "ReflejosInferiorI" }
        }


reflejosInferiorDField =
    Form.textField
        { parser = Ok
        , value = .reflejosInferiorD
        , update = \value values -> { values | reflejosInferiorD = value }
        , attributes = { label = "ReflejosInferiorD", placeholder = "ReflejosInferiorD" }
        }


estadoGeneralField =
    Form.textField
        { parser = Ok
        , value = .estadoGeneral
        , update = \value values -> { values | estadoGeneral = value }
        , attributes = { label = "EstadoGeneral", placeholder = "EstadoGeneral" }
        }


tensionArterialSistolicaField =
    Form.textField
        { parser = String.toInt
        , value = .tensionArterialSistolica
        , update = \value values -> { values | tensionArterialSistolica = value }
        , attributes =
            { label = "TensionArterialSistolica"
            , placeholder = "TensionArterialSistolica"
            }
        }


tensionArterialDiastolicaField =
    Form.textField
        { parser = String.toInt
        , value = .tensionArterialDiastolica
        , update = \value values -> { values | tensionArterialDiastolica = value }
        , attributes =
            { label = "TensionArterialDiastolica"
            , placeholder = "TensionArterialDiastolica"
            }
        }


frecuenciaCardiacaField =
    Form.textField
        { parser = String.toInt
        , value = .frecuenciaCardiaca
        , update = \value values -> { values | frecuenciaCardiaca = value }
        , attributes =
            { label = "FrecuenciaCardiaca"
            , placeholder = "FrecuenciaCardiaca"
            }
        }


frecuenciaRespiratoriaField =
    Form.textField
        { parser = String.toInt
        , value = .frecuenciaRespiratoria
        , update = \value values -> { values | frecuenciaRespiratoria = value }
        , attributes =
            { label = "FrecuenciaRespiratoria"
            , placeholder = "FrecuenciaRespiratoria"
            }
        }


temperaturaField =
    Form.numberField
        { parser = Ok
        , value = .temperatura
        , update = \value values -> { values | temperatura = value }
        , attributes = { label = "Temperatura", placeholder = "Temperatura", step = 1, min = Nothing, max = Nothing }
        }


saturacionOxigenoField =
    Form.textField
        { parser = String.toInt
        , value = .saturacionOxigeno
        , update = \value values -> { values | saturacionOxigeno = value }
        , attributes =
            { label = "SaturacionOxigeno"
            , placeholder = "SaturacionOxigeno"
            }
        }


pesoField =
    Form.numberField
        { parser = Ok
        , value = .peso
        , update = \value values -> { values | peso = value }
        , attributes = { label = "Peso", placeholder = "Peso", step = 1, min = Nothing, max = Nothing }
        }


tallaField =
    Form.numberField
        { parser = Ok
        , value = .talla
        , update = \value values -> { values | talla = value }
        , attributes = { label = "Talla", placeholder = "Talla", step = 1, min = Nothing, max = Nothing }
        }


examenFisicoField =
    Form.textField
        { parser = Ok
        , value = .examenFisico
        , update = \value values -> { values | examenFisico = value }
        , attributes = { label = "ExamenFisico", placeholder = "ExamenFisico" }
        }


analisisConductaField =
    Form.textField
        { parser = Ok
        , value = .analisisConducta
        , update = \value values -> { values | analisisConducta = value }
        , attributes = { label = "AnalisisConducta", placeholder = "AnalisisConducta" }
        }


diagnosticoPrincipalField =
    Form.textField
        { parser = Ok
        , value = .diagnosticoPrincipal
        , update = \value values -> { values | diagnosticoPrincipal = value }
        , attributes = { label = "DiagnosticoPrincipal", placeholder = "DiagnosticoPrincipal" }
        }


diagnosticoRelacionado1Field =
    Form.textField
        { parser = Ok
        , value = .diagnosticoRelacionado1
        , update = \value values -> { values | diagnosticoRelacionado1 = value }
        , attributes = { label = "DiagnosticoRelacionado1", placeholder = "DiagnosticoRelacionado1" }
        }


diagnosticoRelacionado2Field =
    Form.textField
        { parser = Ok
        , value = .diagnosticoRelacionado2
        , update = \value values -> { values | diagnosticoRelacionado2 = value }
        , attributes = { label = "DiagnosticoRelacionado2", placeholder = "DiagnosticoRelacionado2" }
        }


diagnosticoRelacionado3Field =
    Form.textField
        { parser = Ok
        , value = .diagnosticoRelacionado3
        , update = \value values -> { values | diagnosticoRelacionado3 = value }
        , attributes = { label = "DiagnosticoRelacionado3", placeholder = "DiagnosticoRelacionado3" }
        }


form : Form Values Output
form =
    Form.succeed Output
        |> Form.append admitidoPorField
        |> Form.append registroField
        |> Form.append origenAtencionField
        |> Form.append motivoConsultaField
        |> Form.append enfermedadActualField
        |> Form.append antecedentesFamiliaresField
        |> Form.append antecedentesPersonalesField
        |> Form.append revisionSistemasField
        |> Form.append respuestaOcularField
        |> Form.append respuestaVerbalField
        |> Form.append respuestaMotoraField
        |> Form.append fuerzaSuperiorIField
        |> Form.append fuerzaSuperiorDField
        |> Form.append fuerzaInferiorIField
        |> Form.append fuerzaInferiorDField
        |> Form.append reflejosSuperiorIField
        |> Form.append reflejosSuperiorDField
        |> Form.append reflejosInferiorIField
        |> Form.append reflejosInferiorDField
        |> Form.append estadoGeneralField
        |> Form.append tensionArterialSistolicaField
        |> Form.append tensionArterialDiastolicaField
        |> Form.append frecuenciaCardiacaField
        |> Form.append frecuenciaRespiratoriaField
        |> Form.append temperaturaField
        |> Form.append saturacionOxigenoField
        |> Form.append pesoField
        |> Form.append tallaField
        |> Form.append examenFisicoField
        |> Form.append analisisConductaField
        |> Form.append diagnosticoPrincipalField
        |> Form.append diagnosticoRelacionado1Field
        |> Form.append diagnosticoRelacionado2Field
        |> Form.append diagnosticoRelacionado3Field
