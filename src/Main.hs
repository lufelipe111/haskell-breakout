module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

data BreakGame = Game
  { ballPos     :: (Float, Float)
  , ballVel     :: (Float, Float)
  , playerPos   :: Float
  } deriving Show

initialState :: BreakGame
initialState = Game
  { ballPos = (0, 0)
  , ballVel = (0, -150)
  , playerPos = 0
  }

render :: BreakGame -> Picture
render game =
  pictures [ball, mkPaddle  (playerPos game) $ fromIntegral $ -(height `div` 2) + 20]
  where
    ball = uncurry translate (ballPos game) $ color ballColor $ circleSolid 7
    ballColor = white

    mkPaddle :: Float -> Float -> Picture
    mkPaddle x y = pictures
      [ translate x y $ color paddleColor $ rectangleSolid pWidth pHeight
      ]

    paddleColor = light blue

moveBall :: Float -> BreakGame -> BreakGame
moveBall seconds game = game { ballPos = (x', y') }
  where
    (x, y) = ballPos game
    (vx, vy) = ballVel game
    x' = x + vx * seconds
    y' = y + vy * seconds

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

type Radius = Float
type Position = (Float, Float)

wallCollision :: Position -> Radius -> Bool
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

ceilCollision :: Position -> Radius -> Bool
ceilCollision (_, y) radius = y + radius >= fromIntegral (height `div` 2)

ceilBounce :: BreakGame -> BreakGame
ceilBounce game = game { ballVel = (vx, vy') }
  where
    radius = 10
    (vx, vy) = ballVel game
    vy' = if ceilCollision (ballPos game) radius
          then -vy
          else vy

paddleCollision :: Position -> Float -> Radius -> Bool
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

handleKeys :: Event -> BreakGame -> BreakGame
handleKeys (EventKey (Char 'a') _ _ _) game = game { playerPos = pX' }
  where
    pX' = playerPos game - 5
handleKeys (EventKey (Char 'd') _ _ _) game = game { playerPos = pX' }
    where
      pX' = playerPos game + 5
handleKeys _ game = game

update :: Float -> BreakGame -> BreakGame
update seconds = wallBounce . ceilBounce . paddleBounce . moveBall seconds

main :: IO ()
main = play window background fps initialState render handleKeys update

-- width, height, offset, paddleOffset :: Int
-- width = 800
-- height = 400
-- offset = 100
-- paddleOffset = 20

-- window :: Display
-- window = InWindow "Breakout" (width, height) (offset, offset)

-- background :: Color
-- background = black

-- drawing :: Picture
-- drawing = pictures
--   [ translate 0 0 $ color ballColor $ circleSolid 7
--   , translate pPosX pPosY $ color paddleColor $ rectangleSolid 80 5
--   ]
--   where
--     ballColor = white
--     paddleColor = light blue
--     pPosX = 0
--     pPosY = fromIntegral (-(height `div` 2) + paddleOffset)  :: Float

-- main :: IO ()
-- main = display window background drawing
