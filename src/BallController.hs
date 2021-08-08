module BallController where

import Breakout

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
    radius = ballRadius
    (vx, vy) = ballVel game
    vx' = if wallCollision (ballPos game) radius
          then -vx
          else vx

ceilCollision :: Position -> BallRadius -> Bool
ceilCollision (_, y) radius = y + radius >= fromIntegral (height `div` 2)

ceilBounce :: BreakGame -> BreakGame
ceilBounce game = game { ballVel = (vx, vy') }
  where
    radius = ballRadius
    (vx, vy) = ballVel game
    vy' = if ceilCollision (ballPos game) radius
          then -vy
          else vy

paddleCollision :: Position -> Float -> BallRadius -> Bool
paddleCollision (x, y) paddlePos radius =
        y - radius <= topPaddleBorder
    && x          >= leftPaddleBorder
    && x          <= rightPaddleBorder
    where
      leftPaddleBorder  = paddlePos - pWidth / 2
      rightPaddleBorder = paddlePos + pWidth / 2
      topPaddleBorder   = fromIntegral (-(height `div` 2) + paddleOffset) + pHeight / 2

paddleBounce :: BreakGame -> BreakGame
paddleBounce game = game { ballVel = (vx', vy') }
  where
    radius     = ballRadius
    paddlePos  = playerPos game
    (bX, bY)   = ballPos game
    (vx, vy)   = ballVel game
    calcXVel   = ballXVel * (paddlePos - bX)
    (vx', vy') = if paddleCollision (bX, bY) paddlePos radius
                  then (calcXVel / pWidth, -vy)
                  else (vx,  vy)