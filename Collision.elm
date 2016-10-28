module Collision exposing(..)

import Apple exposing (Apple)
import Snake exposing (Snake)

colliding : Snake -> Apple -> Bool
colliding snake apple =
  if snake.x - apple.x < 15
      && snake.x - apple.x > -15
      && snake.y - apple.y < 15
      && snake.y - apple.y > -15
    then True
    else False
