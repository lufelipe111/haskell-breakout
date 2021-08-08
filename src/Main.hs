module Main where

import Graphics.Gloss
import MoveController
import Breakout

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

update :: Float -> BreakGame -> BreakGame
update seconds = wallBounce . ceilBounce . paddleBounce . movePaddle . moveBall seconds

main :: IO ()
main = play window background fps initialState render handleKeys update
