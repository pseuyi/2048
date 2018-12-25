module Main exposing (Game, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Game =
    { grid : List Int
    }


init : () -> ( Game, Cmd Msg )
init _ =
    ( { grid =
            [ 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            ]
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = Start



-- VIEW


view : Game -> Html Msg
view game =
    div
        [ style "background-color" "#5ad455"
        , style "height" "444px"
        , style "width" "444px"
        , style "display" "grid"
        , style "grid-gap" "4px"
        , style "grid-template-columns" "repeat(4, 1fr)"
        , style "border-radius" "4px"
        ]
        (List.map
            (\value ->
                div
                    [ style "background-color" "azure"
                    , style "height" "100px"
                    , style "width" "100px"
                    , style "border" "4px solid azure"
                    , style "color" "black"
                    , style "border-radius" "4px"
                    ]
                    [ text <| String.fromInt value ]
            )
            game.grid
        )



-- UPDATE


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Start ->
            ( { grid = [] }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.none



-- HELPERS


spawnBlock : Game -> Game
spawnBlock game =
    game
