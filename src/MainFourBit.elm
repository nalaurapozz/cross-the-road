module MainFourBit exposing (main)

import Browser
import Html exposing (Html, div, text, textarea)
import Html.Events exposing (onInput)


import FourBit exposing
    ( decode
    , encode
    )


type Msg =
      PlainUpdated String
    | EncodedUpdated String


type alias Model =
    { plain : String
    , encoded : String
    , replain : String
    }


init : Model
init =
    { plain = ""
    , encoded = ""
    , replain = ""
    }


view : Model -> Html Msg
view model =
    div
        []
        [ textarea
            [ onInput PlainUpdated ]
            [ text model.plain ]
        , textarea
            [ onInput EncodedUpdated ]
            [ text model.encoded ]
        , textarea
            []
            [ text model.replain ]
        ]

code : (String -> String) -> String -> String
code coder input =
    input
        |> String.split "\n"
        |> List.map coder
        |> String.join "\n"


update : Msg -> Model -> Model
update msg model =
    case msg of
        PlainUpdated s ->
            { model | plain = s, encoded = code FourBit.encode s }
        EncodedUpdated s ->
            { model | replain = code FourBit.decode s, encoded = s }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
