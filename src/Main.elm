module Main exposing (main)

import Browser


import Flags exposing (Flags)
import Init exposing (init)
import Model exposing (Model, Mode(..))
import Msg exposing (Msg(..))
import Subscriptions exposing (subscriptions)
import Update
import View


main : Program Flags Model Msg
main =
    Browser.element
        { init = Init.init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }
