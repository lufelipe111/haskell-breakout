module Breakout where

import Graphics.Gloss

data BreakGame = Game
  { ballPos       :: (Float, Float)
  , ballVel       :: (Float, Float)
  , playerPos     :: Float
  , leftLongMove  :: Bool
  , rightLongMove :: Bool
  } deriving Show

initialState :: BreakGame
initialState = Game
  { ballPos = (0, 0)
  , ballVel = (0, ballYVel)
  , playerPos = 0
  , leftLongMove = False
  , rightLongMove = False
  }

fps :: Int
fps = 30

width, height, offset, paddleOffset :: Int
width = 800
height = 400
offset = 100
paddleOffset = 20

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