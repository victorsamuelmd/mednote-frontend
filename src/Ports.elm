port module Ports exposing (toJs, showPicker, toElm, storeSession, getSession)

import Json.Encode exposing (Value)


port toJs : String -> Cmd msg


port showPicker : String -> Cmd msg


port storeSession : String -> Cmd msg


port toElm : (String -> msg) -> Sub msg


port getSession : (Value -> msg) -> Sub msg
