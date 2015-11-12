import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window

-- MODEL

(gameWidth,gameHeight) = (600,600)
(halfWidth,halfHeight) = (300,300)

type Direction = Left | Up | Right | Down | Noop

type alias Snake =
  {
    x : Float,
    y : Float,
    d : Direction,
    v : Float
  }

type alias Game =
  {
    snake : Snake
  }

defaultGame : Game
defaultGame =
  {
    snake = Snake 0 0 Up 200
  }

type alias Input =
  {
    dir : Direction,
    delta : Time
  }

-- UPDATE

update : Input -> Game -> Game
update {dir,delta} ({snake} as game) =
  let
    newSnake =
      updateSnakePosition delta <|
      updateSnakeDirection dir snake
  in
    { game |
        snake <- newSnake
    }

updateSnakePosition : Time -> Snake -> Snake
updateSnakePosition time ({x,y,d,v} as snake) =
  case d of
    Left -> { snake | x <- x - 100 * time }
    Up -> { snake | y <- y - 100 * time}
    Right -> { snake | x <- x + 100 * time}
    Down -> { snake | y <- y + 100 * time}

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
view (w,h) {snake} =
  container w h middle <|
  collage gameWidth gameHeight
    [ rect gameWidth gameHeight
        |> filled backgroundColour
    , rect 15 15
        |> make snake
    ]

backgroundColour =
  rgb 204 255 204

make obj shape =
  shape
    |> filled black
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
