module Main where

import Graphics.Gloss
import Breakout
import KeysController
import BallController
import PaddleController

render :: BreakGame -> Picture
render game =
  pictures [ball, mkPaddle  (playerPos game) $ fromIntegral paddleX]
  where
    ball = uncurry translate (ballPos game) $ color ballColor $ circleSolid ballRadius
    ballColor = white

    mkPaddle :: Float -> Float -> Picture
    mkPaddle x y = pictures
      [ translate x y $ color paddleColor $ rectangleSolid pWidth pHeight
      ]

    paddleColor = light blue

update :: Float -> BreakGame -> BreakGame
update seconds = moveBall seconds . wallBounce . ceilBounce . paddleBounce . movePaddle . deadBall

main :: IO ()
main = play window background fps initialState render handleKeys update
