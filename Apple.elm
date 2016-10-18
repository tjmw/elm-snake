module Apple exposing(..)

import Random exposing (..)

import Globals exposing (halfWidth, halfHeight)

--applePositionGenerator = float -(halfWidth) halfWidth

--seed0 = initialSeed 31415
--((initialAppleX, initialAppleY), seed1) = generate (pair applePositionGenerator applePositionGenerator) seed0

type alias Apple =
  {
    x : Float,
    y : Float
    --seed : Seed
  }

--initialApple = Apple initialAppleX initialAppleY seed1

generateNewApple : Apple -> Apple
generateNewApple apple =
  --let
    --((x1, y1), seed1) = generate (pair applePositionGenerator applePositionGenerator) seed
  --in
    Apple (apple.x + 50) (apple.y + 50)
