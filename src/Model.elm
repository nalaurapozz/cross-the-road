module Model exposing (..)


type alias Model =
    { mode : Mode
    , window :
        { width : Float
        , height : Float
        }
    , playerRow : Int  -- bottom row = 0
    , playerX : Float
    , alive : Bool
    , direction : Int
    , celebration : Int
    , movedThisTick : Bool
    , score : Int
    , ticks : Int
    , high_score : Int
    , rows : List Row
    }


type Mode
    = TitleMode
    | PlayMode


type Row
    = RoadRow RoadState
    | TrainRow TrainState
    | RiverRow RiverState
    | PathRow


type alias RoadState =
    { vehicles : List Vehicle
    , speed : Float
    }


type alias TrainState =
    { speed : Float
    , x : Float
    }


type alias RiverState =
    { boats : List Vehicle
    , speed : Float
    }


type alias Vehicle =
    { x : Float
    , sprite : Sprite
    , hitbox : HitBox
    }


type alias Sprite =
    { image : String
    , width : Float
    , height : Float
    }


type alias HitBox =
    List Box


type alias Box =
    { top : Float
    , left : Float
    , width: Float
    , height: Float
    }
