module BallController where

import Breakout

moveBall :: Float -> BreakGame -> BreakGame
moveBall seconds game = game { ballPos = (x', y') }
  where
    radius    = ballRadius
    paddlePos = playerPos game
    (x, y)    = ballPos game
    (vx, vy)  = ballVel game
    x' | paddleCollision (x, y) paddlePos radius && isPaddleSticky game = playerPos game + xDistBallPad game
       | otherwise = x + vx * seconds
    y' | paddleCollision (x, y) paddlePos radius && isPaddleSticky game = y
       | otherwise = y + vy * seconds

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
      topPaddleBorder   = fromIntegral paddleX + pHeight / 2

paddleBounce :: BreakGame -> BreakGame
paddleBounce game = game { ballVel = (vx', vy'), xDistBallPad = dist }
  where
    paddlePos  = playerPos game
    calcXVel   = ballXVel * (paddlePos - bX) / pWidth
    (bX, bY)   = ballPos game
    (vx , vy ) = ballVel game
    (vx', vy') | paddleCollision (bX, bY) paddlePos ballRadius = (calcXVel, abs vy)
               | otherwise = (vx, vy)
    dist       | isBallDead bY paddlePos = 0
               | not (paddleCollision (bX, bY) paddlePos ballRadius) = bX - paddlePos
               | otherwise = xDistBallPad game

isBallDead :: Float -> Float  -> Bool
isBallDead ballPosY paddlePosY = ballPosY < paddlePosY

deadBall :: BreakGame -> BreakGame
deadBall game = game { ballPos = ballPos', isPaddleSticky = isPdSticky' }
  where
    (_, bY) = ballPos game
    ballTopBorder = bY + ballRadius
    bPos' = (playerPos game, fromIntegral paddleX + pHeight / 2 + ballRadius)
    dLine = fromIntegral paddleX
    isPdSticky' | isBallDead ballTopBorder dLine = True
                | otherwise = isPaddleSticky game
    -- xDist'      | isBallDead (bY + ballRadius) (fromIntegral paddleX) = playerPos game
    --             | otherwise = xDistBallPad game
    ballPos'    | isBallDead ballTopBorder dLine = bPos'
                | otherwise = ballPos game