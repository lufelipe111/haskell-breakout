module MoveController where

import Graphics.Gloss.Interface.Pure.Game
import Breakout

handleKeys :: Event -> BreakGame -> BreakGame
handleKeys (EventKey (Char 'a') Down _ _) game = game { leftLongMove = True }
handleKeys (EventKey (Char 'a') Up _ _) game = game { leftLongMove = False }
  -- where
  --   pX' = playerPos game - 5

handleKeys (EventKey (Char 'd') Down _ _) game = game { rightLongMove = True }
handleKeys (EventKey (Char 'd') Up _ _) game = game { rightLongMove = False }
    -- where
    --   pX' = playerPos game + 5

handleKeys _ game = game

movePaddle :: BreakGame -> BreakGame
movePaddle game = game { playerPos = x' }
  where
    x' | leftLongMove game
      && not (rightLongMove game) = playerPos game - 5
       | rightLongMove game
      && not (leftLongMove game)  = playerPos game + 5
       | otherwise                = playerPos game

moveBall :: Float -> BreakGame -> BreakGame
moveBall seconds game = game { ballPos = (x', y') }
  where
    (x, y) = ballPos game
    (vx, vy) = ballVel game
    x' = x + vx * seconds
    y' = y + vy * seconds

type BallRadius = Float
type Position = (Float, Float)

wallCollision :: Position -> BallRadius -> Bool
wallCollision (x, _) radius = leftCollision || rightCollsion
  where
    leftCollision = x - radius <= fromIntegral (-width `div` 2)
    rightCollsion = x + radius >= fromIntegral (width `div` 2)

wallBounce :: BreakGame -> BreakGame
wallBounce game = game { ballVel = (vx', vy) }
  where
    radius = 10
    (vx, vy) = ballVel game
    vx' = if wallCollision (ballPos game) radius
          then -vx
          else vx

ceilCollision :: Position -> BallRadius -> Bool
ceilCollision (_, y) radius = y + radius >= fromIntegral (height `div` 2)

ceilBounce :: BreakGame -> BreakGame
ceilBounce game = game { ballVel = (vx, vy') }
  where
    radius = 10
    (vx, vy) = ballVel game
    vy' = if ceilCollision (ballPos game) radius
          then -vy
          else vy

paddleCollision :: Position -> Float -> BallRadius -> Bool
paddleCollision (x, y) paddlePos radius =
      y - radius <= topPaddleBorder
  && x >= leftPaddleBorder
  && x <= rightPaddleBorder
    where
      leftPaddleBorder = paddlePos - pWidth / 2
      rightPaddleBorder = paddlePos + pWidth / 2
      topPaddleBorder = fromIntegral (-(height `div` 2) + paddleOffset) + pHeight / 2

paddleBounce :: BreakGame -> BreakGame
paddleBounce game = game { ballVel = (vx, vy') }
  where
    radius = 10
    (vx, vy) = ballVel game
    vy' = if paddleCollision (ballPos game) (playerPos game) radius
          then -vy
          else vy