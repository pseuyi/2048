module Main exposing (Game, Msg(..), init, main, subscriptions, update, view)

import Array
import Browser
import Browser.Events
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Grid =
    List Int


type alias Game =
    { grid : Grid
    , spawn : Int
    , direction : Direction
    }


type alias Block =
    { index : Int
    , value : Int
    }


type alias Blocks =
    List Block


type Direction
    = Left
    | Down
    | Right
    | Up
    | None


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowDown" ->
            Down

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        _ ->
            None


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
      , spawn = 1
      , direction = Up
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = Init
    | NewGame Blocks
    | Spawn
    | NewBlock Block
    | ChangeDirection Direction



-- VIEW


view : Game -> Html Msg
view game =
    div
        []
        [ h1 [] [ text "elm 2048" ]
        , button [ onClick Init ] [ text "new game" ]
        , button [ onClick Spawn ] [ text "generate random spawn" ]
        , div [] [ text (String.fromInt game.spawn) ]
        , div [] [ text (printDirection game.direction) ]
        , div
            [ style "background-color" "#5ada55"
            , style "height" "444px"
            , style "width" "444px"
            , style "display" "grid"
            , style "grid-gap" "4px"
            , style "grid-template-columns" "repeat(4, 1fr)"
            , style "border-radius" "4px"
            , style "border" "1px solid black"
            ]
            (List.map
                (\value ->
                    div
                        [ style "background-color" "seashell"
                        , style "height" "100px"
                        , style "width" "100px"
                        , style "border" "4px solid seashell"
                        , style "color" "black"
                        , style "border-radius" "4px"
                        , style "font-size" "88px"
                        , style "text-align" "center"
                        ]
                        [ text <|
                            if value > 0 then
                                String.fromInt value

                            else
                                ""
                        ]
                )
                game.grid
            )
        ]



-- UPDATE


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Init ->
            ( game, initGame )

        Spawn ->
            ( game, spawnBlock )

        NewGame blocks ->
            ( { game | grid = addBlocksToGrid game.grid blocks }
            , Cmd.none
            )

        NewBlock block ->
            ( { game | spawn = block.index }, Cmd.none )

        ChangeDirection direction ->
            ( { game
                | direction = direction
                , grid = collapse game.grid
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Game -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (Decode.map ChangeDirection keyDecoder)



-- HELPERS
-- newGame : Cmd msg
-- newGame =
--     Random.generate NewGame gridWithTwoBlocks
-- gridWithTwoBlocks : Random.Generator List
-- initGame : Cmd Msg
-- initGame =
--     Random.generate NewGame spawnTwoBlocks


initGame : Cmd Msg
initGame =
    Random.generate NewGame initialBlocks



-- spawnTwoBlocks : Random.Generator ( Int, Int )
-- spawnTwoBlocks =
--     Random.pair twoOrFour twoOrFour


initialBlocks : Random.Generator Blocks
initialBlocks =
    Random.list 2 generateBlock


spawnBlock : Cmd Msg
spawnBlock =
    Random.generate NewBlock generateBlock


twoOrFour : Random.Generator Int
twoOrFour =
    Random.map
        (\prob ->
            if prob < 0.5 then
                2

            else
                4
        )
        (Random.float
            0
            1
        )


checkBlockEmpty : Block -> Bool
checkBlockEmpty block =
    block.value == 0


flatten2D : List (List a) -> List a
flatten2D list =
    List.foldr (++) [] list



--
-- addBlockToGrid : Grid -> Block -> Grid
-- addBlockToGrid grid block =
--     let
--         updateBlock curr =
--             if curr.index == block.index then
--                 { curr | value = block.value }
--
--             else
--                 curr
--     in
--     List.map updateBlock grid


generateBlock : Random.Generator Block
generateBlock =
    Random.map2
        (\idx val -> Block idx val)
        generateIndex
        twoOrFour


generateIndex : Random.Generator Int
generateIndex =
    Random.int 0 15


addBlocksToGrid : Grid -> List Block -> Grid
addBlocksToGrid grid blocks =
    let
        flatGrid =
            Array.fromList grid
    in
    Array.toList (List.foldr (\b acc -> Array.set b.index b.value acc) flatGrid blocks)


printDirection : Direction -> String
printDirection dir =
    case dir of
        Left ->
            "Left"

        Down ->
            "Down"

        Right ->
            "Right"

        Up ->
            "Up"

        None ->
            "None"



-- updateGrid : Grid -> Direction -> Grid
-- updateGrid grid dir =
--     let
--      cols = range 0 3
--     in
--       updateColumns grid cols
--
-- updateColumns : Grid -> List Int -> Grid
-- updateColumns grid cols =
--     let
--       flatGrid = Array.fromList grid
--       firstCol = List.map (\n -> n * 4) cols
--
--     in
--       Array.toList (List.foldr (\idx acc -> if Array.set idx v grid) grid rows)
-- moveBlocks : Int -> List Int -> List int
-- moveBlocks idx list =


dbl : Int -> Int
dbl n =
    n * 2


padZeroes : List Int -> List Int
padZeroes list =
    list ++ [ 0, 0, 0, 0 ]


compact : List Int -> List Int
compact list =
    List.filter ((/=) 0) list


collapse : Grid -> Grid
collapse grid =
    let
        -- grab first four ( a row)
        slice =
            grid
                |> List.take 4
                |> compact
                |> padZeroes
                |> List.take 4
    in
    -- recursively collapse grid
    if List.length grid > 4 then
        List.append (collapseSlice slice) (collapse (List.drop 4 grid))

    else
        collapseSlice slice


collapseSlice : List Int -> List Int
collapseSlice slice =
    case slice of
        a :: b :: c :: d :: rest ->
            if a == b && c == d then
                [ dbl a, dbl c, 0, 0 ]

            else if a == b then
                [ dbl a, c, d, 0 ]

            else if b == c then
                [ a, dbl b, 0, 0 ]

            else if c == d then
                [ a, b, dbl c, 0 ]

            else
                slice

        [] ->
            []

        _ ->
            []
