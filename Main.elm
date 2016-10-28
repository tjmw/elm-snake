import AnimationFrame
import Color exposing (..)
import Debug
import Collage exposing (..)
import Element exposing (..)
import Globals exposing (halfWidth, halfHeight)
import Html exposing (..)
import Html.App as App
import Keyboard
import Random
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
    apple = Apple 10 10,
    score = 0,
    size = Size 0 0
  }

init : (Game, Cmd Msg)
init = (defaultGame, Task.perform (\_ -> NoOp) Resize (Window.size))

type Msg
  = Resize Size
  | ChangeDirection Snake.Direction
  | Tick Time
  | Collide (Int, Int)
  | NoOp

-- UPDATE

update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    NoOp -> (game, Cmd.none)
    Resize size -> ({ game | size = size }, Cmd.none)
    ChangeDirection direction ->
      let
        {snake} = game
        snake1 = updateSnakeDirection direction snake
      in
        ({ game | snake = snake1 }, Cmd.none)
    Tick delta ->
      let
        {snake,apple,score} = game

        snake1 = updateSnakePosition delta snake

        eatingAnApple = colliding snake1 apple

        cmd =
          if eatingAnApple
             then Random.generate Collide (Random.map2 (,) (Random.int (round -(halfWidth)) (round halfWidth)) (Random.int (round -(halfWidth)) (round halfWidth)))
             else Cmd.none
      in
        ({ game | snake = snake1 }, cmd)
    Collide coords ->
      ({ game |
          apple = Apple (toFloat <| fst coords) (toFloat <| snd coords),
          score = game.score + 1,
          snake = increaseVelocity game.snake
      }, Cmd.none)

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
    , update = update
    , subscriptions = subscriptions
    }
