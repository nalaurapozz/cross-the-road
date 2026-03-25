module SetTimer exposing (setTimer)


import Process
import Task


import Msg exposing (Msg(..))
import TickLength exposing (tickLength)


setTimer : Cmd Msg
setTimer =
    Task.perform (\_ -> Tick) (Process.sleep tickLength)
