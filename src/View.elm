module View exposing (view)


import Html exposing (Attribute, Html, br, button, div, img, p, text)
import Html.Attributes exposing (class, src, style, value)
import Html.Events exposing (preventDefaultOn, stopPropagationOn)
import Html.Lazy exposing (..)
import Json.Decode as D
import Model exposing (..)
import Msg exposing (Msg(..))
import RowNum exposing (rowNum)
import TickLength exposing (tickLength)


view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ class "playingarea"
            , style "width" (model.window.width |> px)
            , style "height" (model.window.height |> px)
            , nonDefaultOn "touchstart" MovePlayer
            , nonPropagatingOn "mousedown" MovePlayer
            ]
            [ lazy3 viewScores
                model.window.height
                model.score
                model.high_score
            , div
                []
                ( List.indexedMap
                    (viewRow model (List.length model.rows))
                    model.rows
                )
            , lazy instructions model.window.height
            ]
        , lazy6 viewGameOver
            model.alive
            model.mode
            model.window.width
            model.window.height
            model.score
            model.ticks
        ]


instructions : Float -> Html Msg
instructions height =
    div
        []
        [ div
            [ class "instructions"
            , style "bottom" "0.7em"
            , style "left" "0.6em"
            , style "font-size" (height / 30 |> px)
            ]
            [text "Any key/button to move"]
        ]


px : Float -> String
px f =
    String.fromFloat(f) ++ "px"


playerImage : Model -> String
playerImage model =
    if model.alive then
        if model.celebration > 0 then
            "images/player_won.svg"
        else if model.direction > 0 then
            "images/player_up.svg"
        else
            "images/player_down.svg"
    else
        "images/splat.svg"


trainImage : Model -> String
trainImage model =
    "images/train.svg"


trainSignImage : Float -> String
trainSignImage x =
    if x > -1 && x > 1 && ((round (x * 5)) |> modBy 2) == 0 then
        "train-sign-active.svg"
    else
        "train-sign-inactive.svg"


viewRow : Model -> Int -> Int -> Row -> Html Msg
viewRow model numRows rowIdx row =
    let
        height : Float
        height = model.window.height / (toFloat numRows)

        playerOnThisRow : Bool
        playerOnThisRow = rowNum numRows rowIdx == model.playerRow

        player : List (Html Msg)
        player =
            if playerOnThisRow && model.mode /= TitleMode then
                let
                    playerLeft : Float
                    playerLeft =
                        (model.playerX * model.window.height) - (height/2)
                in
                    [ img
                        [ class "player"
                        , src (playerImage model)
                        , style "left" (playerLeft |> px)
                        , style "height" (height |> px)
                        ]
                        []
                    ]
            else
                []
    in
        case row of
            RoadRow road -> viewRoadRow road model player height
            RiverRow river -> viewRiverRow river model player height
            TrainRow train -> viewTrainRow train model player height
            PathRow -> viewPathRow model player height


viewRiverRow : RiverState -> Model -> List (Html Msg) -> Float -> Html Msg
viewRiverRow river model player height =
    div
        [ class "river row"
        , style "height" (height |> px)
        ]
        (  List.map (viewBoat model height) river.boats
        ++ player
        )


viewRoadRow : RoadState -> Model -> List (Html Msg) -> Float -> Html Msg
viewRoadRow road model player height =
    div
        [ class "road row"
        , style "height" (height |> px)
        ]
        ( List.map (viewVehicle model height) road.vehicles
        ++ player
        )


viewTrainRow : TrainState -> Model -> List (Html Msg) -> Float -> Html Msg
viewTrainRow train model player height =
    let
        trainLeft : Float
        trainLeft =
            (train.x * model.window.height) - (height/2)
    in
        div
            [ class "track row"
            , style "height" (height |> px)
            ]
            (  [ img
                    [ class "train-sign"
                    , style "height" (height |> px)
                    , src ("images/" ++ trainSignImage train.x)
                    ]
                    []
                , img
                    [ class "train"
                    , src (trainImage model)
                    , style "left" (trainLeft |> px)
                    , style "height" (height |> px)
                    ]
                    []
                ]
                ++ player
            )


viewPathRow : Model -> List (Html Msg) -> Float -> Html Msg
viewPathRow model player height =
    div
        [ class "path row"
        , style "height" (height |> px)
        ]
        player


viewVehicle : Model -> Float -> Vehicle -> Html Msg
viewVehicle model height vehicle =
    let
        left : Float
        left = (vehicle.x * model.window.height) - (height / 2)
    in
        img
            [ class "vehicle"
            , src ("images/" ++ vehicle.sprite.image)
            , style "left" (left |> px)
            , style "height" (height |> px)
            ]
            []


viewBoat : Model -> Float -> Vehicle -> Html Msg
viewBoat model height boat =
    let
        left : Float
        left = (boat.x * model.window.height) - (height / 2)
    in
        img
            [ class "boat"
            , src ("images/" ++ boat.sprite.image)
            , style "left" (left |> px)
            , style "height" (height |> px)
            ]
            []


nonPropagatingOn : String -> Msg -> Attribute Msg
nonPropagatingOn event msg =
    stopPropagationOn event (D.map (\m -> (m, True)) (D.succeed msg))


nonDefaultOn : String -> Msg -> Attribute Msg
nonDefaultOn event msg =
    preventDefaultOn event (D.map (\m -> (m, True)) (D.succeed msg))


eventSwallower : String -> Msg -> Attribute Msg
eventSwallower event msg =
    Html.Events.custom
        event
        ( D.succeed
            { message = msg
            , stopPropagation = False
            , preventDefault = True
            }
        )


type alias GameOverText =
    { message1 : String
    , message2 : String
    , button_message : String
    }


gameOverText : Mode -> Int -> Int -> GameOverText
gameOverText mode score ticks =
    if mode == TitleMode then
        { message1 = "Welcome to Cross The Road!"
        , message2 = "Try to get across, avoiding cars and riding on boats."
        , button_message = "Start game"
        }
    else
        { message1 = "Game over, well played!"
        , message2 =
            (  "Score: "
            ++ String.fromInt score
            ++ timeScore score ticks
            )
        , button_message = "Play again"
        }



viewGameOver : Bool -> Mode -> Float -> Float -> Int -> Int -> Html Msg
viewGameOver alive mode width height score ticks =
    if alive && mode /= TitleMode then
        div [] []
    else
        let
            left = width * 0.25
            w = width * 0.5
            top = height * 0.1
            font = height * 0.05
            gotext = gameOverText mode score ticks
        in
            div
                [ class "gameover"
                , style "left" (left |> px)
                , style "width" (w |> px)
                , style "top" (top |> px)
                , style "font-size" (font |> px)
                ]
                [ p [] [ text gotext.message1 ]
                , p
                    []
                    [ button
                        [nonPropagatingOn "click" StartGame]
                        [text gotext.button_message]
                    ]
                , p [] [ text gotext.message2 ]
                ]


timeScore : Int -> Int-> String
timeScore score ticks =
    if score == 0 then
        ""
    else
        " ("
        ++ String.fromInt (round tickLength * ticks // score)
        ++ "ms/point)"


viewScores : Float -> Int -> Int -> Html Msg
viewScores height score high_score =
    div
        []
        [ div
            [ class "scores highscore"
            , style "font-size" (height / 21 |> px)
            ]
            [ text ("High Score: " ++ (String.fromInt high_score)) ]
        , div
            [ class "scores score"
            , style "font-size" (height / 21 |> px)
            ]
            [ text ("Score: " ++ (String.fromInt score)) ]
        ]
