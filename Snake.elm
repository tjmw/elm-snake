module Snake where

import Time exposing (..)

import Globals exposing (halfWidth, halfHeight)

velocityStep = 10

type Direction = Left | Up | Right | Down | Noop

type alias Snake =
  {
    x : Float,
    y : Float,
    d : Direction,
    v : Float
  }

initialSnake = Snake 0 0 Up 150

increaseVelocity : Snake -> Snake
increaseVelocity ({v} as snake) =
  { snake | v = v + velocityStep }

updateSnakePosition : Time -> Snake -> Snake
updateSnakePosition time ({x,y,d,v} as snake) =
  case d of
    Left ->
      if x < -(halfWidth) then
        { snake | x = halfWidth }
      else
        { snake | x = x - v * time }

    Down ->
      if y < -(halfHeight) then
        { snake | y = halfHeight }
      else
        { snake | y = y - v * time }

    Right ->
      if x > halfWidth then
        { snake | x = -(halfWidth) }
      else
        { snake | x = x + v * time }

    Up ->
      if y > halfHeight then
        { snake | y = -(halfHeight) }
      else
        { snake | y = y + v * time }

    _ -> snake

updateSnakeDirection : Direction -> Snake -> Snake
updateSnakeDirection direction ({x,y,d,v} as snake) =
  case d of
    Left -> case direction of
      Up -> { snake | d = direction }
      Down -> { snake | d = direction }
      _ -> { snake | d = d }

    Up -> case direction of
      Left -> { snake | d = direction }
      Right -> { snake | d = direction }
      _ -> { snake | d = d }

    Right -> case direction of
      Up -> { snake | d = direction }
      Down -> { snake | d = direction }
      _ -> { snake | d = d }

    Down -> case direction of
      Left -> { snake | d = direction }
      Right -> { snake | d = direction }
      _ -> { snake | d = d }

    _ -> snake
