module Subscriptions exposing (subscriptions)


import Browser.Events exposing (onKeyDown, onResize)
import Json.Decode as D
import Model exposing (Model)
import Msg exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        resize width height =
            WindowSize (toFloat width) (toFloat height)

        keydown =
            D.succeed MovePlayer
    in
        Sub.batch
            [ onResize resize
            , onKeyDown keydown
            ]
