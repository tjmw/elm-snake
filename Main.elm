import AnimationFrame
import Color exposing (..)
import Debug
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.App as App
import Keyboard
import Task
import Text exposing (..)
import Time exposing (..)
import Window exposing (Size)

import Apple exposing (
  Apple,
  generateNewApple)

import Collision exposing (colliding)

import Snake exposing (
  Snake,
  increaseVelocity,
  initialSnake,
  updateSnakePosition,
  updateSnakeDirection)

import Globals exposing(
  gameWidth,
  gameHeight,
  halfWidth,
  halfHeight)

-- MODEL

type alias Game =
  {
    snake : Snake,
    apple : Apple,
    score : Int,
    size : Size
  }

defaultGame : Game
defaultGame =
  {
    snake = initialSnake,
    apple = Apple 100 100,
    score = 0,
    size = Size 0 0
  }

init : ( Game, Cmd Msg )
init = (defaultGame, Task.perform (\_ -> NoOp) Resize (Window.size))

type Msg
  = Resize Size
  | ChangeDirection Snake.Direction
  | Tick Time
  | NoOp

-- UPDATE

update : Msg -> Game -> Game
update msg game =
  case msg of
    NoOp -> game
    Resize size -> { game | size = size }
    ChangeDirection direction ->
      let
        {snake} = game
        snake1 = updateSnakeDirection direction snake
      in
        { game | snake = snake1 }
    Tick delta ->
      let
        {snake,apple,score} = game

        snake1 = updateSnakePosition delta snake

        eatingAnApple = colliding snake1 apple

        snake2 =
          if eatingAnApple
            then increaseVelocity snake1
            else snake1

        apple1 =
          if eatingAnApple
            then generateNewApple apple
            else apple

        score1 =
          if eatingAnApple
            then score + 1
            else score

      in
        { game |
            snake = snake2,
            apple = apple1,
            score = score1
        }

-- VIEW

txt f = leftAligned << f << monospace << Text.color textColour << fromString

textColour = rgb 150 150 150
backgroundColour = rgb 204 255 204

view : Game -> Html Msg
view ({snake, apple, score, size} as game) =
  let scoreEl : Element
      scoreEl = toString score |> txt (Text.height 50)
      dbg = Debug.log "Game state" game
  in
    toHtml <|
    container size.width size.height middle <|
    collage gameWidth gameHeight
      [
        rect gameWidth gameHeight |> filled backgroundColour,
        toForm scoreEl |> move (0, gameHeight/2 - 40),
        make apple (circle 8) red,
        make snake (rect 15 15) black
      ]

make obj shape color =
  shape
    |> filled color
    |> move (obj.x,obj.y)

-- SUBSCRIPTIONS

subscriptions : Game -> Sub Msg
subscriptions game =
  Sub.batch
    [ Window.resizes Resize
    , Keyboard.downs keyCodeToMsg
    , AnimationFrame.diffs (Tick<<inSeconds)
    ]

keyCodeToMsg keyCode =
  case keyCode of
    37 -> ChangeDirection Snake.Left
    38 -> ChangeDirection Snake.Up
    39 -> ChangeDirection Snake.Right
    40 -> ChangeDirection Snake.Down
    _ -> NoOp

-- MAIN

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = \msg m -> update msg m ! []
    , subscriptions = subscriptions
    }

--gameState : Signal Game
--gameState =
  --Signal.foldp update defaultGame input

--delta =
  --Signal.map inSeconds (fps 35)

--input : Signal Input
--input =
  --Signal.sampleOn delta <|
    --Signal.map2 Input
      --(Signal.map arrowToDirection Keyboard.arrows)
      --delta
