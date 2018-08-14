module Helpers exposing (..)

import Json.Decode as JsDecode
import Date exposing (Date)


dateDecoder : JsDecode.Decoder Date
dateDecoder =
    JsDecode.string
        |> JsDecode.andThen
            (\dateString ->
                case (Date.fromString dateString) of
                    Ok date ->
                        JsDecode.succeed date

                    Err errorString ->
                        JsDecode.fail errorString
            )
