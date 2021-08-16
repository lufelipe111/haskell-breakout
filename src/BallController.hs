module BallController where

import Breakout
import TileModel
import Control.Parallel.Strategies

moveBall :: Float -> BreakGame -> BreakGame
moveBall seconds game = game'
  where
    radius    = ballRadius
    paddlePos = playerPos game
    (x, y)    = ballPos game
    (vx, vy)  = ballVel game
    x' | paddleCollision (x, y) paddlePos radius && isPaddleSticky game = playerPos game + xDistBallPad game
       | otherwise = x + vx * seconds
    y' | paddleCollision (x, y) paddlePos radius && isPaddleSticky game = y
       | otherwise = y + vy * seconds
    game' | game == GameOver = game
          | otherwise = game { ballPos = (x', y') }

type BallRadius = Float
type Position = (Float, Float)

wallCollision :: Position -> BallRadius -> Bool
wallCollision (x, _) radius = leftCollision || rightCollsion
  where
    leftCollision = x - radius <= fromIntegral (-width `div` 2)
    rightCollsion = x + radius >= fromIntegral (width `div` 2)

wallBounce :: BreakGame -> BreakGame
wallBounce game = game'
  where
    radius = ballRadius
    (vx, vy) = ballVel game
    vx' = if wallCollision (ballPos game) radius
          then -vx
          else vx
    game' | game == GameOver = game
          | otherwise = game { ballVel = (vx', vy) }

ceilCollision :: Position -> BallRadius -> Bool
ceilCollision (_, y) radius = y + radius >= fromIntegral (height `div` 2)

ceilBounce :: BreakGame -> BreakGame
ceilBounce game = game'
  where
    radius = ballRadius
    (vx, vy) = ballVel game
    vy' = if ceilCollision (ballPos game) radius
          then -vy
          else vy
    game' | game == GameOver = game
          | otherwise = game { ballVel = (vx, vy') }

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
paddleBounce game = game'
  where
    paddlePos  = playerPos game
    calcXVel   = ballXVel * (paddlePos - bX) / pWidth
    (bX, bY)   = ballPos game
    (vx , vy ) = ballVel game
    (vx', vy') | paddleCollision (bX, bY) paddlePos ballRadius = (calcXVel, abs vy)
               | otherwise = (vx, vy)
    dist       | not (paddleCollision (bX, bY) paddlePos ballRadius) = bX - paddlePos
               | otherwise = xDistBallPad game
    game'      | game == GameOver = game
               | otherwise = game { ballVel = (vx', vy'), xDistBallPad = dist }

isBallDead :: Float -> Float  -> Bool
isBallDead ballPosY paddlePosY = ballPosY < paddlePosY

deadBall :: BreakGame -> BreakGame
deadBall game = game'
  where
    (_, bY) = ballPos game
    ballTopBorder = bY + ballRadius
    bPos' = (playerPos game, fromIntegral paddleX + pHeight / 2 + ballRadius)
    dLine = fromIntegral paddleX
    isPdSticky' 
      | isBallDead ballTopBorder dLine = True
      | otherwise = isPaddleSticky game

    xDist'      
      | isBallDead (bY + ballRadius) (fromIntegral paddleX) = 0
      | otherwise = xDistBallPad game

    ballPos'    
      | isBallDead ballTopBorder dLine = bPos'
      | otherwise = ballPos game

    lifes'      
      | isBallDead ballTopBorder dLine = lifes game - 1
      | otherwise = lifes game

    game'       
      | (game /= GameOver) && lifes game >= 0 = 
          game { ballPos = ballPos', isPaddleSticky = isPdSticky', xDistBallPad = xDist', lifes = lifes' }
      | otherwise = GameOver  

topTileCollision :: (Float, Float) -> Float -> Tile -> Bool
topTileCollision (x, y) radius t =
     y - radius <= tileTopBorder
  && y - radius >= tileBottomBorder
  && x - radius <= tileRightBorder
  && x + radius >= tileLeftBorder
  && y > tileTopBorder
  && x < tileRightBorder
  && x > tileLeftBorder
  where
    (tXPos, tYPos)    = tilePos t
    tileTopBorder     = tYPos + (tileHeight t / 2)
    tileBottomBorder  = tYPos - (tileHeight t / 2)
    tileRightBorder   = tXPos + (tileWidth t  / 2)
    tileLeftBorder    = tXPos - (tileWidth t  / 2)

leftTileCollision :: (Float, Float) -> Float -> Tile -> Bool
leftTileCollision (x, y) radius t =
     y + radius >= tileBottomBorder
  && y - radius <= tileTopBorder
  && x + radius <= tileRightBorder
  && x + radius >= tileLeftBorder
  && x < tileLeftBorder
  && y < tileTopBorder
  && y > tileBottomBorder
  where
    (tXPos, tYPos)    = tilePos t
    tileTopBorder     = tYPos + (tileHeight t / 2)
    tileBottomBorder  = tYPos - (tileHeight t / 2)
    tileRightBorder   = tXPos + (tileWidth t  / 2)
    tileLeftBorder    = tXPos - (tileWidth t  / 2)


bottomTileCollision :: (Float, Float) -> Float -> Tile -> Bool
bottomTileCollision (x, y) radius t =
     y + radius >= tileBottomBorder
  && y + radius <= tileTopBorder
  && x - radius <= tileRightBorder
  && x + radius >= tileLeftBorder
  && y < tileBottomBorder
  && x < tileRightBorder
  && x > tileLeftBorder
  where
    (tXPos, tYPos)    = tilePos t
    tileTopBorder     = tYPos + (tileHeight t / 2)
    tileBottomBorder  = tYPos - (tileHeight t / 2)
    tileRightBorder   = tXPos + (tileWidth t  / 2)
    tileLeftBorder    = tXPos - (tileWidth t  / 2)


rightTileCollision :: (Float, Float) -> Float -> Tile -> Bool
rightTileCollision (x, y) radius t =
     y + radius >= tileBottomBorder
  && y - radius <= tileTopBorder
  && x - radius <= tileRightBorder
  && x - radius >= tileLeftBorder
  && x > tileRightBorder
  && y > tileBottomBorder
  && y < tileTopBorder
  where
    (tXPos, tYPos)    = tilePos t
    tileTopBorder     = tYPos + (tileHeight t / 2)
    tileBottomBorder  = tYPos - (tileHeight t / 2)
    tileRightBorder   = tXPos + (tileWidth t  / 2)
    tileLeftBorder    = tXPos - (tileWidth t  / 2)


tileBounce :: BreakGame -> BreakGame
tileBounce game = game'
  where
    (vx, vy) = ballVel game
    (x , y ) = ballPos game
    bottomCollisionList = parMap rpar (bottomTileCollision (ballPos game) ballRadius) (tiles game)
    topCollisionList    = parMap rpar (topTileCollision    (ballPos game) ballRadius) (tiles game)
    leftCollisionList   = parMap rpar (leftTileCollision   (ballPos game) ballRadius) (tiles game)
    rightCollisionList  = parMap rpar (rightTileCollision  (ballPos game) ballRadius) (tiles game)
    vx' | or leftCollisionList  = -(abs vx)
        | or rightCollisionList =   abs vx
        | otherwise = vx
    vy' | or bottomCollisionList = -(abs vy)
        | or topCollisionList    =   abs vy
        | otherwise = vy
    x'  | or leftCollisionList  = x - 0.1                     -- Line to prevent double collision at once
        | or rightCollisionList = x + 0.1                     -- Line to prevent double collision at once
        | otherwise = x
    y'  | or bottomCollisionList = y - 0.1                    -- Line to prevent double collision at once
        | or topCollisionList    = y + 0.1                    -- Line to prevent double collision at once
        | otherwise = y
    p'  | any cond (tiles game) = points game + 10            -- Add 10 points per collision
        | otherwise = points game
    tiles' = map (decreaseTileState cond) (tiles game)        -- decrease state of the hitted tiles
    tiles''= filter (\t' -> tileState t' >= 0) tiles'         -- remove tiles com states 0 ou menor
    cond t = bottomTileCollision (ballPos game) ballRadius t  -- if there was a collision
          || topTileCollision    (ballPos game) ballRadius t
          || leftTileCollision   (ballPos game) ballRadius t
          || rightTileCollision  (ballPos game) ballRadius t
    game' | game == GameOver = game
          | otherwise = game { ballPos = (x', y'), ballVel = (vx', vy'), tiles = tiles'', points = p'}


decreaseTileState :: (Tile -> Bool) -> Tile -> Tile
decreaseTileState cond t
  | cond t = t { tileState = tileState t - 1 }
  | otherwise = t

ballController :: BreakGame -> BreakGame
ballController = deadBall . tileBounce . wallBounce . ceilBounce . paddleBounce