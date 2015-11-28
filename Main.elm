import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Random exposing (..)
import Text
import Time exposing (..)
import Window

-- MODEL

(gameWidth,gameHeight) = (600,600)
(halfWidth,halfHeight) = (gameWidth/2,gameHeight/2)

applePositionGenerator = float -(halfWidth) halfWidth

seed0 = initialSeed 31415
((initialAppleX, initialAppleY), seed1) = generate (pair applePositionGenerator applePositionGenerator) seed0

type Direction = Left | Up | Right | Down | Noop

type alias Snake =
  {
    x : Float,
    y : Float,
    d : Direction,
    v : Float
  }

type alias Apple =
  {
    x : Float,
    y : Float
  }

type alias Game =
  {
    snake : Snake,
    apple : Apple,
    seed : Seed
  }

defaultGame : Game
defaultGame =
  {
    snake = Snake 0 0 Up 150,
    apple = Apple initialAppleX initialAppleY,
    seed = seed1
  }

type alias Input =
  {
    dir : Direction,
    delta : Time
  }

-- UPDATE

update : Input -> Game -> Game
update {dir,delta} game =
  let
    {snake,apple,seed} = game

    snake1 =
      updateSnakePosition delta <|
      updateSnakeDirection dir snake

    (apple1, seed1) =
      if colliding snake1 apple
        then generateNewApple seed
        else (apple, seed)

  in
    { game |
        snake <- snake1,
        apple <- apple1,
        seed <- seed1
    }

generateNewApple : Seed -> (Apple, Seed)
generateNewApple seed =
  let
    ((x1, y1), seed1) = generate (pair applePositionGenerator applePositionGenerator) seed
  in
    (Apple x1 y1, seed1)

colliding : Snake -> Apple -> Bool
colliding snake apple =
  if snake.x - apple.x < 15
      && snake.x - apple.x > -15
      && snake.y - apple.y < 15
      && snake.y - apple.y > -15
    then True
    else False

updateSnakePosition : Time -> Snake -> Snake
updateSnakePosition time ({x,y,d,v} as snake) =
  case d of
    Left ->
      if x < -(halfWidth) then
        { snake | x <- halfWidth }
      else
        { snake | x <- x - v * time }

    Up ->
      if y < -(halfHeight) then
        { snake | y <- halfHeight }
      else
        { snake | y <- y - v * time }

    Right ->
      if x > halfWidth then
        { snake | x <- -(halfWidth) }
      else
        { snake | x <- x + v * time }

    Down ->
      if y > halfHeight then
        { snake | y <- -(halfHeight) }
      else
        { snake | y <- y + v * time }

updateSnakeDirection : Direction -> Snake -> Snake
updateSnakeDirection direction ({x,y,d,v} as snake) =
  case d of
    Left -> case direction of
      Up -> { snake | d <- direction }
      Down -> { snake | d <- direction }
      _ -> { snake | d <- d }

    Up -> case direction of
      Left -> { snake | d <- direction }
      Right -> { snake | d <- direction }
      _ -> { snake | d <- d }

    Right -> case direction of
      Up -> { snake | d <- direction }
      Down -> { snake | d <- direction }
      _ -> { snake | d <- d }

    Down -> case direction of
      Left -> { snake | d <- direction }
      Right -> { snake | d <- direction }
      _ -> { snake | d <- d }

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

arrowToDirection : { x : Int, y : Int } -> Direction
arrowToDirection arrow =
  case (arrow.x, arrow.y) of
    (1,0) -> Right
    (-1,0) -> Left
    (0,1) -> Down
    (0,-1) -> Up
    _ -> Noop

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map2 Input
      (Signal.map arrowToDirection Keyboard.arrows)
      delta
