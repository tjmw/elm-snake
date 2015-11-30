import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text exposing (..)
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
    apple : Apple,
    score : Int
  }

defaultGame : Game
defaultGame =
  {
    snake = initialSnake,
    apple = initialApple,
    score = 0
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
    {snake,apple,score} = game

    snake1 =
      updateSnakePosition delta <|
      updateSnakeDirection dir snake

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

view : (Int,Int) -> Game -> Element
view (w,h) ({snake, apple, score} as game) =
  Debug.watchSummary "Game state" (\_ -> game) <|
  let scoreEl : Element
      scoreEl = toString score |> txt (Text.height 50)
  in
    container w h middle <|
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
    (-1,0) -> Snake.Left
    (0,1) -> Snake.Up
    (1,0) -> Snake.Right
    (0,-1) -> Snake.Down
    _ -> Snake.Noop

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map2 Input
      (Signal.map arrowToDirection Keyboard.arrows)
      delta
