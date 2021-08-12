module Breakout where

import Graphics.Gloss
import TileModel

data BreakGame = Game
  { ballPos        :: (Float, Float)
  , ballVel        :: (Float, Float)
  , playerPos      :: Float
  , playerVel      :: Float
  , leftLongMove   :: Bool
  , rightLongMove  :: Bool
  , isPaddleSticky :: Bool
  , xDistBallPad   :: Float
  , tiles          :: [Tile]
  } deriving Show

xTilesNum :: Integer
xTilesNum = 15
yTilesNum :: Integer
yTilesNum = 5
tilesCenterDist :: Integer
tilesCenterDist = 35
tWid ::Float
tWid = 30
tHgt :: Float
tHgt = 10

initialState :: BreakGame
initialState = Game
  { ballPos        = (0, 0)
  , ballVel        = (0, ballYVel)
  , playerPos      = 0
  , playerVel      = 5
  , leftLongMove   = False
  , rightLongMove  = False
  , isPaddleSticky = False
  , xDistBallPad   = 0
  , tiles          = generateTiles xTilesNum yTilesNum
  }

testTiles :: [Tile]
testTiles = [Tile { tileColor  = red
                  , tilePos    = (15, 20)
                  , tileState  = 1
                  , tileHeight = tHgt
                  , tileWidth  = tWid
                  },
                  Tile { tileColor  = blue
                  , tilePos    = (50, 20)
                  , tileState  = 1
                  , tileHeight = tHgt
                  , tileWidth  = tWid
                  } 
            ]

generateTiles :: Integer -> Integer -> [Tile]
generateTiles x y | y == 0 =    []
                  | x == 1 =    Tile { tileColor = makeColor 1 1 1 1
                                     , tilePos = (xPos, yPos)
                                     , tileState = 0
                                     , tileHeight = tHgt
                                     , tileWidth = tWid
                                     } : generateTiles xTilesNum (y-1)
                  | otherwise = Tile { tileColor = makeColor 1 1 1 1
                                      , tilePos = (xPos, yPos)
                                      , tileState = 1
                                      , tileHeight = tHgt
                                      , tileWidth = tWid
                                      } : generateTiles (x-1) y
  where
    xPos = fromIntegral ((-xTilesNum-2) * tilesCenterDist `div` 2 + fromIntegral x * tilesCenterDist + margin)
    yPos = fromIntegral (y * 20)
    margin = 20

fps :: Int
fps = 30

width, height, offset, paddleFloorDist, paddleX :: Int
width = 800
height = 400
offset = 100
paddleFloorDist = 20
paddleX = -(height `div` 2) + paddleFloorDist

pWidth, pHeight :: Float
pWidth = 80
pHeight = 5

window :: Display
window = InWindow "Breakout" (width, height) (offset, offset)

background :: Color
background = black

ballRadius :: Float
ballRadius = 7


ballXVel :: Float
ballXVel = -300

ballYVel :: Float
ballYVel = -150

deadOffset :: Float
deadOffset = 50