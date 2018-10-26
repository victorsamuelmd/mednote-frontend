port module Ports exposing (gotSession, storeSession)

import Json.Encode exposing (Value)


port storeSession : String -> Cmd msg


port gotSession : (Value -> msg) -> Sub msg
