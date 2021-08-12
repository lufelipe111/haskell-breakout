module Main where

import Graphics.Gloss
import Breakout
import KeysController
import BallController
import PaddleController
import TileModel
import Control.Parallel.Strategies

-- Convert a world to a picture
render :: BreakGame -> [Picture] -> Picture
render game tileBMPs  =
  pictures [ ball
           , mkPaddle (playerPos game) $ fromIntegral paddleX
           , mkTiles (tiles game)
           ]
  where
    ball = uncurry translate (ballPos game) $ color ballColor $ circleSolid ballRadius
    ballColor = white

    mkPaddle :: Float -> Float -> Picture
    mkPaddle x y = pictures
      [ translate x y $ color paddleColor $ rectangleSolid pWidth pHeight
      ]
    paddleColor = light blue

    mkTiles :: [Tile] -> Picture
    mkTiles ts = pictures $ parMap rpar (tilePic tileBMPs) ts

-- look at what and the order that need to be validated and updated
update :: Float -> BreakGame -> BreakGame
update seconds = moveBall seconds . movePaddle . wallBounce . ceilBounce . tileBounce . paddleBounce . deadBall

main :: IO ()
main = do
  blueTile      <- loadBMP "assets/01-Breakout-Tiles.bmp"
  blueBreakTile <- loadBMP "assets/02-Breakout-Tiles.bmp"
  play window background fps initialState (`render` [blueBreakTile, blueTile]) handleKeys update
