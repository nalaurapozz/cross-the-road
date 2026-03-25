module PlayerPos exposing (playerPos)


import Model exposing (Model)


playerPos : Model -> Float
playerPos model =
    (model.window.width / 2) / model.window.height
