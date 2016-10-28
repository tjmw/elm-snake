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

import Apple exposing (Apple)
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
    size : Size,
    paused : Bool
  }

defaultGame : Game
defaultGame =
  {
    snake = initialSnake,
    apple = Apple -50 -50,
    score = 0,
    size = Size 0 0,
    paused = False
  }

init : (Game, Cmd Msg)
init = (defaultGame, Task.perform (\_ -> NoOp) Resize (Window.size))

type Msg
  = Resize Size
  | ChangeDirection Snake.Direction
  | Tick Time
  | Collide (Int, Int)
  | Pause
  | NoOp

randomCoordGenerator : Random.Generator Int
randomCoordGenerator = Random.int (round -(halfWidth)) (round halfWidth)

collideCmdWithRandomTuple : Cmd Msg
collideCmdWithRandomTuple =
  Random.generate Collide <| Random.map2 (,) randomCoordGenerator randomCoordGenerator

togglePaused : Game -> Game
togglePaused game =
  { game | paused = not game.paused }

-- UPDATE

update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  if game.paused then
    case msg of
      Pause -> (togglePaused game, Cmd.none)
      _ -> (game, Cmd.none)
  else
    case msg of
      NoOp -> (game, Cmd.none)
      Pause -> (togglePaused game, Cmd.none)
      Resize size -> ({ game | size = size }, Cmd.none)
      ChangeDirection direction ->
        let
          {snake} = game
          snake_ = updateSnakeDirection direction snake
        in
          ({ game | snake = snake_ }, Cmd.none)
      Tick delta ->
        let
          {snake,apple,score} = game

          snake_ = updateSnakePosition delta snake

          eatingAnApple = colliding snake_ apple

          cmd =
            if eatingAnApple then collideCmdWithRandomTuple
            else Cmd.none
        in
          ({ game | snake = snake_ }, cmd)
      Collide (x, y) ->
        ({ game |
            apple = Apple (toFloat <| x) (toFloat <| y),
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
    80 -> Pause
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
