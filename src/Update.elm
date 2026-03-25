module Update exposing (update)


import Http
import Model exposing (..)
import Msg exposing (Msg(..))
import PlayerPos exposing (playerPos)
import RowNum exposing (rowNum)
import SetTimer exposing (setTimer)
import TickLength exposing (tickLength)
import Task


screen_left = -1
screen_right = 2


type PlayerChange =
      PlayerUnchanged
    | PlayerMoved Float
    | PlayerDead


combinePlayerChanges : List PlayerChange -> PlayerChange
combinePlayerChanges changes =
    let
        comb : PlayerChange -> PlayerChange -> PlayerChange
        comb c1 c2 =
            case c1 of
                PlayerUnchanged -> c2
                _ -> c1
    in
        List.foldr comb PlayerUnchanged changes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( startGame model
            , Http.post
                { url = "https://smolpxl.artificialworlds.net/s/event.php"
                , body =
                      Http.stringBody
                          "text/plain; charset=UTF-8"
                          "cross-the-road.played"
                , expect = Http.expectWhatever EventPosted
                }
            )
        WindowSize width height ->
            ( windowSize width height model, Cmd.none )
        Tick ->
            ( tick model, setTimer )
        MovePlayer ->
            ( movePlayer model, Cmd.none )
        EventPosted _ ->
            ( model, Cmd.none )


startGame : Model -> Model
startGame model =
    { model
    | mode = PlayMode
    , playerRow = 0
    , playerX = playerPos model
    , alive = True
    , direction = 1
    , score = 0
    , ticks = 0
    , celebration = 0
    , high_score =
        if model.score > model.high_score then
            model.score
        else
            model.high_score
    , movedThisTick = False
    }


windowSize : Float -> Float -> Model -> Model
windowSize width height model =
    let
        window = model.window
        ratio = width / height
        h =
            if ratio < 0.75 then
                width / 0.75
            else
                height
        w =
            if ratio > 2.0 then
                height * 2.0
            else
                width
    in
        { model
        | window =
            { window
            | width = w * 0.85
            , height = h * 0.85
            }
        }


vehiclePos : Float -> Vehicle -> Float
vehiclePos speed vehicle =
    let
        x1 = vehicle.x + speed * tickLength
        x2 = if x1 < screen_left then screen_right else x1
    in
        if x2 > screen_right then screen_left else x2


tick : Model -> Model
tick model =
    if not model.alive then
        { model | movedThisTick = False }
    else
        let
            rows_changes : List ( Row, PlayerChange )
            rows_changes = List.indexedMap (tickRow model) model.rows
            changes: PlayerChange
            changes = combinePlayerChanges (List.map Tuple.second rows_changes)
        in
            { model
            | rows = List.map Tuple.first rows_changes
            , alive =
                case changes of
                    PlayerDead -> False
                    _ -> model.alive
            , playerX =
                case changes of
                    PlayerMoved x -> model.playerX + x
                    _ -> model.playerX
            , celebration =
                if model.celebration > 0 then
                    model.celebration - 1
                else
                    model.celebration
            , movedThisTick =
                False
            , ticks =
                model.ticks + 1
            }


tickRow : Model -> Int -> Row -> ( Row, PlayerChange )
tickRow model rowIdx row =
    let
        row_n =
            (rowNum (List.length model.rows) rowIdx)
    in
        case row of
            RoadRow road ->
                let
                    ( r, player_change ) = tickRoad model row_n road
                in
                    ( RoadRow r, player_change )
            RiverRow river ->
                let
                    ( r, player_change ) = tickRiver model row_n river
                in
                    ( RiverRow r, player_change )
            TrainRow train ->
                let
                    ( r, player_change ) = tickTrain model row_n train
                in
                    ( TrainRow r, player_change )
            PathRow ->
                ( PathRow, PlayerUnchanged )


tickRoad : Model -> Int -> RoadState -> ( RoadState, PlayerChange )
tickRoad model row_n road =
    let
        vehicles_changes =
            List.map
                (tickVehicle model row_n road.speed)
                road.vehicles
    in
        ( { road
          | vehicles = List.map Tuple.first vehicles_changes
          }
        , combinePlayerChanges (List.map Tuple.second vehicles_changes)
        )


tickRiver : Model -> Int -> RiverState -> ( RiverState, PlayerChange )
tickRiver model row_n river =
    let
        boats : List Vehicle
        boats = List.map (tickBoat model row_n river.speed) river.boats

        boat_poses : List Float
        boat_poses = List.map (vehiclePos river.speed) river.boats

        player_change : PlayerChange
        player_change =
            if List.any (playerIsOnBoat model row_n) boat_poses then
                PlayerMoved (river.speed * tickLength)
            else if row_n == model.playerRow then
                PlayerDead
            else
                PlayerUnchanged
    in
        ( { river
          | boats = boats
          }
        , player_change
        )


playerIsOnBoat : Model -> Int -> Float -> Bool
playerIsOnBoat model row_n vehicleX =
       ((abs (vehicleX - model.playerX)) < 0.1)
    && model.playerRow == row_n


playerIsHit : Model -> Int -> Float -> Bool
playerIsHit model row_n vehicleX =
       ((abs (vehicleX - model.playerX)) < 0.12)
    && model.playerRow == row_n


playerIsHitByTrain : Model -> Int -> Float -> Bool
playerIsHitByTrain model row_n trainX =
       ((abs ((trainX + 0.12) - model.playerX)) < 0.24)
    && model.playerRow == row_n


tickVehicle : Model -> Int -> Float -> Vehicle -> ( Vehicle, PlayerChange )
tickVehicle model row_n speed vehicle =
    let
        vp = vehiclePos speed vehicle
        player_change =
            if playerIsHit model row_n vp then
                PlayerDead
            else
                PlayerUnchanged
    in
        ( { vehicle
          | x = vp
          }
        , player_change
        )


tickBoat : Model -> Int -> Float -> Vehicle -> Vehicle
tickBoat model row_n speed boat =
    { boat
    | x = vehiclePos speed boat
    }


tickTrain : Model -> Int-> TrainState -> ( TrainState, PlayerChange )
tickTrain model row_n train =
    let
        x1 = train.x + train.speed * tickLength
        x2 = if x1 < -7 then 5 else x1
        x3 = if x2 > 5 then -7 else x2
        player_change =
            if playerIsHitByTrain model row_n x3 then
                PlayerDead
            else
                PlayerUnchanged
    in
        ( { train | x = x3 }, player_change )


movePlayer : Model -> Model
movePlayer model =
    if model.alive && not model.movedThisTick then
        let
            playerRow = model.playerRow + model.direction
            top = (playerRow == (List.length model.rows) - 1)
            bottom = (playerRow == 0)
            won = top || bottom
            celebration = if won then 30 else model.celebration
            score = if won then model.score + 1 else model.score
            direction =
                if top then
                    -1
                else if bottom then
                    1
                else
                    model.direction
            playerX =
                if won then playerPos model else model.playerX
        in
            { model
            | playerRow = playerRow
            , celebration = celebration
            , score = score
            , direction = direction
            , playerX = playerX
            , movedThisTick = True
            }
    else
        model
