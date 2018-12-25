module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

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


type alias Model =
    { grid : List Int
    }


init : () -> ( Model, Cmd Msg )
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
    = Expand
    | Collapse



-- VIEW


view : Model -> Html Msg
view model =
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
            model.grid
        )



-- div
--     [ style "background-color" "#5ad455"
--     , style "height" "800px"
--     , style "width" "800px"
--     ]
--     [ if model then
--         div
--             []
--             [ button [ onClick Collapse ] [ text "Collapse" ]
--             , text "Widget"
--             , text <| .name dog
--             ]
--
--       else
--         div
--             []
--             [ button [ onClick Expand ] [ text "Expand" ] ]
--     ]
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Expand ->
            ( { grid = [] }, Cmd.none )

        Collapse ->
            ( { grid = [] }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
