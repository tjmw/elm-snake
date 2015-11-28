import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window

import Apple exposing (
  Apple,
  initialApple,
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
    apple : Apple
  }

defaultGame : Game
defaultGame =
  {
    snake = initialSnake,
    apple = initialApple
  }

type alias Input =
  {
    dir : Snake.Direction,
    delta : Time
  }

-- UPDATE

update : Input -> Game -> Game
update {dir,delta} game =
  let
    {snake,apple} = game

    snake1 =
      updateSnakePosition delta <|
      updateSnakeDirection dir snake

    snake2 =
      if colliding snake1 apple
        then increaseVelocity snake1
        else snake1

    apple1 =
      if colliding snake2 apple
        then generateNewApple apple
        else apple

  in
    { game |
        snake <- snake2,
        apple <- apple1
    }

-- VIEW

view : (Int,Int) -> Game -> Element
view (w,h) {snake, apple} =
  container w h middle <|
  collage gameWidth gameHeight
    [
      rect gameWidth gameHeight |> filled backgroundColour,
      make apple (circle 8) red,
      make snake (rect 15 15) black
    ]

backgroundColour =
  rgb 204 255 204

make obj shape color =
  shape
    |> filled color
    |> move (obj.x,obj.y)

-- SIGNALS

main =
  Signal.map2 view Window.dimensions gameState

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

delta =
  Signal.map inSeconds (fps 35)

arrowToDirection : { x : Int, y : Int } -> Snake.Direction
arrowToDirection arrow =
  case (arrow.x, arrow.y) of
    (1,0) -> Snake.Right
    (-1,0) -> Snake.Left
    (0,1) -> Snake.Down
    (0,-1) -> Snake.Up
    _ -> Snake.Noop

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map2 Input
      (Signal.map arrowToDirection Keyboard.arrows)
      delta
