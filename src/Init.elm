module Init exposing (init, car1)


import Browser.Dom exposing (getViewport, Viewport)
import Flags exposing (Flags)
import Model exposing (..)
import Msg exposing (Msg(..))
import SetTimer exposing (setTimer)
import Task


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { mode = TitleMode
            , window =
                { width = 320.0
                , height = 240.0
                }
            , playerRow =
                0
            , playerX =
                0.0
            , alive =
                True
            , direction =
                1
            , celebration =
                0
            , movedThisTick =
                False
            , score =
                0
            , ticks =
                0
            , high_score =
                0
            , rows =
                [ PathRow
                , RiverRow
                    { boats =
                        [ { raft_2_right | x = 0.5 }
                        , { raft_1 | x = 0.6 }
                        , { raft_1 | x = 1.0 }
                        , { raft_1 | x = 1.1 }
                        , { raft_3_right | x = 1.5 }
                        , { raft_1 | x = -0.9 }
                        , { raft_1 | x = -0.8 }
                        ]
                    , speed = -0.0002
                    }
                , RiverRow
                    { boats =
                        [ { raft_1 | x = 0.5 }
                        , { raft_1 | x = 0.8 }
                        , { raft_1 | x = 1.0 }
                        , { raft_1 | x = 1.2 }
                        , { raft_1 | x = 1.5 }
                        , { raft_1 | x = 0.0 }
                        , { raft_1 | x = 0.3 }
                        , { raft_1 | x = -0.9 }
                        , { raft_1 | x = -0.8 }
                        , { raft_1 | x = -0.2 }
                        ]
                    , speed = 0.0002
                    }
                , TrainRow
                    { speed = -0.0011
                    , x = -1
                    }
                , RoadRow
                    { vehicles =
                        [ car2_right
                        ]
                    , speed = 0.0005
                    }
                , RoadRow
                    { vehicles =
                        [ car1_right
                        ]
                    , speed = 0.0008
                    }
                , RoadRow
                    { vehicles =
                        [ car1
                        ]
                    , speed = -0.0008
                    }
                , RoadRow
                    { vehicles =
                        [ car3_left
                        , car4_left
                        ]
                    , speed = -0.0005
                    }
                , PathRow
                ]
            }
        resizeForViewport : Viewport -> Msg
        resizeForViewport viewport =
            WindowSize viewport.viewport.width viewport.viewport.height
    in
        ( model
        , Cmd.batch
            [ Task.perform resizeForViewport getViewport
            , setTimer
            ]
        )


dummy_hitbox =
    [
        { top = -10
        , left = -10
        , width = 20
        , height = 20
        }
    ]


car1 : Vehicle
car1 =
    { x = 0.6
    , sprite =
        { image = "car1.svg"
        , width = 32.0
        , height = 32.0
        }
    , hitbox =
        dummy_hitbox
    }


car1_right : Vehicle
car1_right =
    { x = 0.6
    , sprite =
        { image = "car1_right.svg"
        , width = 32.0
        , height = 32.0
        }
    , hitbox =
        dummy_hitbox
    }



car2_right : Vehicle
car2_right =
    { x = 0.8
    , sprite =
        { image = "car2_right.png"
        , width = 32.0
        , height = 32.0
        }
    , hitbox =
        dummy_hitbox
    }


car3_left : Vehicle
car3_left =
    { x = 0.8
    , sprite =
        { image = "car3_left.png"
        , width = 32.0
        , height = 32.0
        }
    , hitbox =
        dummy_hitbox
    }


car4_left : Vehicle
car4_left =
    { x = 0.0
    , sprite =
        { image = "car4_left.png"
        , width = 32.0
        , height = 32.0
        }
    , hitbox =
        dummy_hitbox
    }


raft_1 : Vehicle
raft_1 =
    { x = 0.6
    , sprite =
        { image = "raft_1.svg"
        , width = 32.0
        , height = 32.0
        }
    , hitbox =
        dummy_hitbox
    }


raft_2_right : Vehicle
raft_2_right =
    { x = 0.6
    , sprite =
        { image = "raft_2_right.svg"
        , width = 32.0
        , height = 32.0
        }
    , hitbox =
        dummy_hitbox
    }


raft_3_right : Vehicle
raft_3_right =
    { x = 0.6
    , sprite =
        { image = "raft_3_right.svg"
        , width = 32.0
        , height = 32.0
        }
    , hitbox =
        dummy_hitbox
    }
