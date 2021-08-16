module Main where

import Graphics.Gloss
import Breakout
import KeysController
import BallController
import PaddleController
import TileModel

-- Convert a world to a picture
render :: BreakGame -> [Picture] -> Picture
render game tileBMPs
  | game == Start = startScene
  | game == GameOver = gameOverScene
  | otherwise = inGameScene

  where
    ball = uncurry translate (ballPos game) $ color ballColor $ circleSolid ballRadius
    ballColor = white
    pointsPic = color white $ translate (-380) 150 $ scale 0.12 0.12 $ text $ "Points: " ++ show (points game)
    lifesPic  = color white $ translate   250  150 $ scale 0.12 0.12 $ text $ "Lifes: " ++ show (lifes game)
    walls = pictures[ translate (-(fromIntegral width / 2)) 0 $ color white $ rectangleSolid 3 (fromIntegral height)
                    , translate   (fromIntegral width / 2 ) 0 $ color white $ rectangleSolid 3 (fromIntegral height)
                    , translate 0 (-(fromIntegral height / 2))$ color white $ rectangleSolid (fromIntegral width)  3
                    , translate 0   (fromIntegral height / 2) $ color white $ rectangleSolid (fromIntegral width)  3
                    ]

    mkPaddle :: Float -> Float -> Picture
    mkPaddle x y = pictures
      [ translate x y $ color paddleColor $ rectangleSolid pWidth pHeight
      ]
    paddleColor = light blue

    mkTiles :: [Tile] -> Picture
    mkTiles ts = pictures $ map (tilePic tileBMPs) ts

    -- Start Scene
    startScene = pictures [ color white $ translate (-105)  70   $ scale 0.4 0.4   $ text "Breakout"
                          , color white $ translate (-195)(-50 ) $ scale 0.2 0.2   $ text "Press [Enter] to start game"
                          , color white $ translate (-95) (-180) $ scale 0.14 0.14 $ text "Developed by Azzolini"
                          , walls
                          ]

    -- In-Game Scene
    inGameScene = pictures [ ball
                           , mkPaddle (playerPos game) $ fromIntegral paddleX
                           , mkTiles (tiles game)
                           , pointsPic
                           , walls
                           , lifesPic
                           ]

    -- Game Over Scene
    gameOverScene = pictures [ color white $ translate (-105)  70   $ scale 0.4 0.4   $ text "Game Over"
                             , color white $ translate (-195)(-50 ) $ scale 0.2 0.2   $ text "Press [Enter] to start game"
                             , color white $ translate (-95) (-180) $ scale 0.14 0.14 $ text "Developed by Azzolini"
                             , walls
                             ]




-- look at what and the order that need to be validated and updated
update :: Float -> BreakGame -> BreakGame
update seconds game | game == Start || game == GameOver = game
                    | otherwise = moveBall seconds . movePaddle . ballController $ game

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
  play window background fps startState (`render` tilePics) handleKeys update