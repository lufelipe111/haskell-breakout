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
update seconds = moveBall seconds . movePaddle . ballController

main :: IO ()
main = do
  blueTile        <- loadBMP "assets/01-Breakout-Tiles.bmp"
  blueBreakTile   <- loadBMP "assets/02-Breakout-Tiles.bmp"
  greenTile       <- loadBMP "assets/03-Breakout-Tiles.bmp"
  greenBreakTile  <- loadBMP "assets/04-Breakout-Tiles.bmp"
  purpleTile      <- loadBMP "assets/05-Breakout-Tiles.bmp"
  purpleBreakTile <- loadBMP "assets/06-Breakout-Tiles.bmp"
  redTile         <- loadBMP "assets/07-Breakout-Tiles.bmp"
  redBreakTile    <- loadBMP "assets/08-Breakout-Tiles.bmp"
  orangeTile      <- loadBMP "assets/09-Breakout-Tiles.bmp"
  orangeBreakTile <- loadBMP "assets/10-Breakout-Tiles.bmp"
  lBlueTile       <- loadBMP "assets/11-Breakout-Tiles.bmp"
  lBlueBreakTile  <- loadBMP "assets/12-Breakout-Tiles.bmp"
  yellowTile      <- loadBMP "assets/13-Breakout-Tiles.bmp"
  yellowBreakTile <- loadBMP "assets/14-Breakout-Tiles.bmp"
  dGreenTile      <- loadBMP "assets/15-Breakout-Tiles.bmp"
  dGreenBreakTile <- loadBMP "assets/16-Breakout-Tiles.bmp"
  grayTile        <- loadBMP "assets/17-Breakout-Tiles.bmp"
  grayBreakTile   <- loadBMP "assets/18-Breakout-Tiles.bmp"
  brownTile       <- loadBMP "assets/19-Breakout-Tiles.bmp"
  brownBreakTile  <- loadBMP "assets/20-Breakout-Tiles.bmp"
  let tilePics = [ blueTile
                 , blueBreakTile
                 , greenTile
                 , greenBreakTile
                 , purpleTile
                 , purpleBreakTile
                 , redTile
                 , redBreakTile
                 , orangeTile
                 , orangeBreakTile
                 , lBlueTile
                 , lBlueBreakTile
                 , yellowTile
                 , yellowBreakTile
                 , dGreenTile
                 , dGreenBreakTile
                 , grayTile
                 , grayBreakTile
                 , brownTile
                 , brownBreakTile]
  play window background fps initialState (`render` tilePics) handleKeys update