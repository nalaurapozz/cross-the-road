module UpdateTests exposing (..)


import Test exposing (describe,test,Test)
import Expect exposing (FloatingPointTolerance(..))


import Init exposing (init, car1)
import Model exposing (..)
import Msg exposing (Msg(..))
import Update exposing (update)


t1 = test  "StartGame sets mode to PlayMode" <|
    let
        m = { model | mode = TitleMode }
        u = Tuple.first (update StartGame m)
    in
        \() -> Expect.equal u.mode PlayMode


t1a = test  "StartGame resets player" <|
    let
        m =
            { model
            | mode = PlayMode
            , alive = False
            , playerRow = 3
            , direction = -1
            , playerX = -10.0
            , celebration = 29
            , movedThisTick = True
            , score = 250
            , ticks = 23
            }
        u = Tuple.first (update StartGame m)
    in
        Expect.all
            [ \() -> Expect.equal u.alive True
            , \() -> Expect.equal u.playerRow 0
            , \() -> Expect.equal u.direction 1
            , nearlyEqual u.playerX (Just 0.6666666)
            , \() -> Expect.equal u.celebration 0
            , \() -> Expect.equal u.movedThisTick False
            , \() -> Expect.equal u.score 0
            , \() -> Expect.equal u.ticks 0
            ]


t2 = test "WindowSize changes width and height" <|
    let
        u = Tuple.first (update ( WindowSize 134 128 ) model)
    in
        Expect.all
            [ nearlyEqual u.window.height (Just 108.8)
            , nearlyEqual u.window.width (Just 113.89999)
            ]


t2a = test "A wide window is constrained to 2:1 ratio" <|
    let
        u = Tuple.first (update ( WindowSize 1000 100 ) model)
    in
        \() -> Expect.equal u.window { width = 170.0, height = 85.0 }


t2b = test "A tall window is constrained to 3:4 ratio" <|
    let
        u = Tuple.first (update ( WindowSize 100 1000 ) model)
    in
        \() -> Expect.equal
            u.window
            { width = 85.0, height = 0.85 * (100.0/0.75) }


t3 = test "Tick moves the cars in a road" <|
    let
        m =
            { playingModel
            | rows = [ road -0.001 [ {car1|x=1.5}, {car1|x=0.2} ] ]
            }
        u = Tuple.first (update Tick m)
    in
        Expect.all
            [ nearlyEqual 1.45 ((firstCar u) |> Maybe.map .x)
            , nearlyEqual 0.15 ((secondCar u) |> Maybe.map .x)
            ]


t3a = test "Player on a path does not die" <|
    let
        m =
            { playingModel
            | rows =
                [ river 0.001 [ car1 ]
                , road 0.001 [ {car1|x=0.8} ]
                , PathRow
                ]
            , playerRow = 0
            }
        u = Tuple.first (update Tick m)
    in
        \() -> Expect.true "Player should be alive" u.alive


t4 = test "Cars past the left go back to the right" <|
    let
        m =
            { playingModel
            | rows = [ road -0.001 [ {car1|x=-1} ] ]
            }
        u = Tuple.first (update Tick m)
    in
        Expect.all
            [ nearlyEqual 2 ((firstCar u) |> Maybe.map .x)
            ]


t5 = test "Cars past the right go back to the left" <|
    let
        m =
            { playingModel
            | rows = [ road 0.001 [ {car1|x=2} ] ]
            }
        u = Tuple.first (update Tick m)
    in
        Expect.all
            [ nearlyEqual -1 ((firstCar u) |> Maybe.map .x)
            ]


t6 = test "Pressing a key moves player to next row" <|
    let
        m = { model | playerRow = 0 }
        u = Tuple.first (update MovePlayer m)
    in
        \() -> Expect.equal u.playerRow 1


t6a = test "Player does not move when dead" <|
    let
        m = { model | playerRow = 1, alive = False }
        u = Tuple.first (update MovePlayer m)
    in
        Expect.all
            [ \() -> Expect.equal u.playerRow 1
            , \() -> Expect.equal u.alive False
            ]


t6b = test "Player only moves once before a tick" <|
    let
        m =
            { playingModel
            | rows = [ path, path, path, path ]
            , playerRow = 0
            }
        move mdl = Tuple.first (update MovePlayer mdl)
        u = move (move (move m))
    in
        \() -> Expect.equal 1 u.playerRow


t6c = test "Player can move again after a tick" <|
    let
        m =
            { playingModel
            | rows = [ path, path, path, path ]
            , playerRow = 0
            }
        move mdl = Tuple.first (update MovePlayer mdl)
        tk mdl = Tuple.first (update Tick mdl)
        u = move (tk (move m))
    in
        \() -> Expect.equal 2 u.playerRow


t7 = test "Tick when a car is touching player kills" <|
    let
        m =
            { playingModel
            | rows = [ road 0.001 [ {car1|x=0.6} ] ]
            , playerRow = 0
            }
        u = Tuple.first (update Tick m)
    in
        \() -> Expect.false "Player should be dead" u.alive


t7z = test "High score updates on game start" <|
    let
        m =
            { playingModel
            | rows = [ road 0.001 [ {car1|x=0.6} ] ]
            , playerRow = 0
            , alive = False
            , score = 3
            , high_score = 1
            }
        u = Tuple.first (update StartGame m)
    in
        \() -> Expect.equal 3 u.high_score


t7y = test "High score does not change if score is low" <|
    let
        m =
            { playingModel
            | rows = [ road 0.001 [ {car1|x=0.6} ] ]
            , playerRow = 0
            , alive = False
            , score = 3
            , high_score = 4
            }
        u = Tuple.first (update StartGame m)
    in
        \() -> Expect.equal 4 u.high_score


t7a = test "Tick when a car is just touching player's left kills" <|
    let
        m =
            { playingModel
            | rows = [ road 0.001 [ {car1|x=0.5} ] ]
            , playerRow = 0
            }
        u = Tuple.first (update Tick m)
    in
        \() -> Expect.false "Player should be dead" u.alive


t7b = test "Tick when a car is just touching player's right kills" <|
    let
        m =
            { playingModel
            | rows = [ road 0.001 [ {car1|x=0.72} ] ]
            , playerRow = 0
            }
        u = Tuple.first (update Tick m)
    in
        \() -> Expect.false "Player should be dead" u.alive


t7c = test "Tick when train is touching player kills" <|
    let
        m =
            { playingModel
            | rows = [ train 0.001 0.6 ]
            , playerRow = 0
            }
        u = Tuple.first (update Tick m)
    in
        \() -> Expect.false "Player should be dead" u.alive


t8 = test "Tick when a car is left of the player does not kill" <|
    let
        m =
            { playingModel
            | rows = [ road 0.001 [ {car1|x=0.4} ] ]
            , playerRow = 0
            }
        u = Tuple.first (update Tick m)
    in
        \() -> Expect.true "Player should be alive" u.alive


t9 = test "Tick when a car is right of the player does not kill" <|
    let
        m =
            { playingModel
            | rows = [ road 0.001 [ {car1|x=0.8} ] ]
            , playerRow = 0
            }
        u = Tuple.first (update Tick m)
    in
        \() -> Expect.true "Player should be alive" u.alive


t10 = test "Tick when a car is in a different row does not kill" <|
    let
        m =
            { playingModel
            | rows =
                [ road 0.001 [ {car1|x=1.0} ]
                , road 0.001 []
                , road 0.001 [ {car1|x=1.0} ]
                ]
            , playerRow = 1
            }
        u = Tuple.first (update Tick m)
    in
        \() -> Expect.true "Player should be alive" u.alive


t11 = test "Player stays dead when dead" <|
    let
        m =
            { playingModel
            | alive = False
            }
        u = Tuple.first (update Tick m)
    in
        \() -> Expect.false "Player should still be dead" u.alive


t12 = test "Vehicles do not move if player is dead" <|
    let
        m =
            { playingModel
            | rows = [ road 0.001 [ {car1|x=1.2} ] ]
            , alive = False
            }
        u = Tuple.first (update Tick m)
    in
        Expect.all
            [ nearlyEqual 1.2 ((firstCar u) |> Maybe.map .x)
            ]


t13 = test "Player dies in water" <|
    let
        m =
            { playingModel
            | rows = [ river 0.001 [] ]
            }
        u = Tuple.first (update Tick m)
    in
        \() -> Expect.false "Player should be dead" u.alive


t14 = test "Player moves with a boat" <|
    let
        m =
            { playingModel
            | rows = [ river -0.001 [ {car1|x=1.5} ] ]
            , playerRow = 0
            , playerX = 1.5
            }
        u = Tuple.first (update Tick m)
    in
         nearlyEqual 1.45 (Just u.playerX)


t15 = test "Player starts in the middle" <|
    nearlyEqual 0.66666 (Just playingModel.playerX)


t16 = test "Celebration starts when we reach top row" <|
    let
        m =
            { playingModel
            | rows = [ path, path ]
            , playerRow = 0
            , score = 1
            }
        u = Tuple.first (update MovePlayer m)
    in
        \() -> Expect.equal 30 u.celebration


t16a = test "Score increases when we reach top row" <|
    let
        m =
            { playingModel
            | rows = [ path, path ]
            , playerRow = 0
            , score = 1
            }
        u = Tuple.first (update MovePlayer m)
    in
        \() -> Expect.equal 2 u.score


t16b = test "Direction reverses when we reach top row" <|
    let
        m =
            { playingModel
            | rows = [ path, path ]
            , playerRow = 0
            , direction = 1
            }
        u = Tuple.first (update MovePlayer m)
    in
        \() -> Expect.equal -1 u.direction


t16b1 = test "Position resets when we reach top row" <|
    let
        m =
            { playingModel
            | rows = [ path, path ]
            , playerRow = 0
            , playerX = 0.1
            , score = 1
            }
        u = Tuple.first (update MovePlayer m)
    in
        nearlyEqual 0.6666 (Just u.playerX)


t16c = test "Direction reverses when we reach bottom row" <|
    let
        m =
            { playingModel
            | rows = [ path, path, path ]
            , playerRow = 1
            , direction = -1
            }
        u = Tuple.first (update MovePlayer m)
    in
        \() -> Expect.equal 1 u.direction


t16d = test "Celebration starts when we reach bottom row" <|
    let
        m =
            { playingModel
            | rows = [ path, path ]
            , playerRow = 1
            , score = 1
            , direction = -1
            }
        u = Tuple.first (update MovePlayer m)
    in
        \() -> Expect.equal 30 u.celebration


t16d1 = test "Position resets when we reach bottom row" <|
    let
        m =
            { playingModel
            | rows = [ path, path ]
            , playerRow = 1
            , playerX = 0.9
            , direction = -1
            }
        u = Tuple.first (update MovePlayer m)
    in
        nearlyEqual 0.6666 (Just u.playerX)


t16e = test "Score increases when we reach bottom row" <|
    let
        m =
            { playingModel
            | rows = [ path, path ]
            , playerRow = 1
            , score = 1
            , direction = -1
            }
        u = Tuple.first (update MovePlayer m)
    in
        \() -> Expect.equal 2 u.score


t16f = test "Score does not increase when we reach normal row" <|
    let
        m =
            { playingModel
            | rows = [ path, path, path ]
            , playerRow = 2
            , score = 1
            , direction = -1
            }
        u = Tuple.first (update MovePlayer m)
    in
        \() -> Expect.equal 1 u.score


t17 = test "Celebration reduces on tick" <|
    let
        m =
            { playingModel
            | rows = [ path, path ]
            , playerRow = 0
            , celebration = 5
            }
        u = Tuple.first (update Tick m)
    in
         \() -> Expect.equal 4 u.celebration


t18 = test "Celebration stops reducing at 0" <|
    let
        m =
            { playingModel
            | rows = [ path, path ]
            , playerRow = 0
            , celebration = 0
            }
        u = Tuple.first (update Tick m)
    in
         \() -> Expect.equal 0 u.celebration


-- ---


model : Model
model =
    Tuple.first (init ())


playingModel : Model
playingModel =
    Tuple.first (update StartGame model)

path : Row
path =
    PathRow

road : Float -> List Vehicle -> Row
road speed vehicles =
    RoadRow { vehicles = vehicles, speed = speed }


river : Float -> List Vehicle -> Row
river speed boats =
    RiverRow { boats = boats, speed = speed }


train : Float -> Float -> Row
train speed x =
    TrainRow { speed = speed, x = x }


firstCar : Model -> Maybe Vehicle
firstCar m =
    List.head m.rows |>
        Maybe.andThen
            (\r ->
                case r of
                    RoadRow rr -> List.head rr.vehicles
                    _ -> Nothing
            )


secondCar : Model -> Maybe Vehicle
secondCar m =
    case m.rows of
        RoadRow r::_ ->
            case r.vehicles of
                _::car::_ -> Just car
                _ -> Nothing
        _ -> Nothing


nearlyEqual : Float -> Maybe Float -> (() -> Expect.Expectation)
nearlyEqual n1 n2 =
    \() ->
        case n2 of
            Nothing ->
                Expect.fail "Value does not exist"
            Just n ->
                Expect.within (Absolute 0.001) n1 n
