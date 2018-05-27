module Util exposing (..)

import Task exposing (perform)
import Pointer exposing (onDown)


relativePos : Pointer.Event -> ( Float, Float )
relativePos event =
    event.pointer.offsetPos


sendMessage : a -> Cmd a
sendMessage x =
    Task.succeed x
        |> Task.perform identity
