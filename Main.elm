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
    snake = Snake halfWidth halfHeight Up 200
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
      updateSnake delta dir snake
  in
    { game |
        snake <- newSnake
    }


updateSnake : Time -> Direction -> Snake -> Snake
updateSnake time direction ({x,y,d,v} as snake) =
  case direction of
    Left -> { snake | x <- x - 10 }
    Up -> { snake | y <- y - 10 }
    Right -> { snake | x <- x + 10 }
    Down -> { snake | y <- y + 10 }
    Noop -> { snake | y <- y, x <- x }

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
