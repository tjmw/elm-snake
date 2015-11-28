module Apple where

import Random exposing (..)

import Globals exposing (halfWidth, halfHeight)

applePositionGenerator = float -(halfWidth) halfWidth

seed0 = initialSeed 31415
((initialAppleX, initialAppleY), seed1) = generate (pair applePositionGenerator applePositionGenerator) seed0

type alias Apple =
  {
    x : Float,
    y : Float,
    seed : Seed
  }

initialApple = Apple initialAppleX initialAppleY seed1

generateNewApple : Apple -> Apple
generateNewApple ({seed} as apple) =
  let
    ((x1, y1), seed1) = generate (pair applePositionGenerator applePositionGenerator) seed
  in
    Apple x1 y1 seed1
