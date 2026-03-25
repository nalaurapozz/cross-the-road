module Msg exposing (Msg(..))


import Http


type Msg
    = StartGame
    | WindowSize Float Float
    | Tick
    | MovePlayer
    | EventPosted (Result Http.Error ())
