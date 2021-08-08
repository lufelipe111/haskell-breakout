module PaddleController where

import Breakout

movePaddle :: BreakGame -> BreakGame
movePaddle game = game { playerPos = x' }
  where
    leftBorder = playerPos game - (pWidth / 2)
    rightBorder = playerPos game + (pWidth / 2)
    x' | leftLongMove game  
         && not (rightLongMove game) 
         && leftBorder > fromIntegral (-width `div` 2) = playerPos game - 5
       | rightLongMove game 
         && not (leftLongMove  game)
         && rightBorder < fromIntegral (width `div` 2) = playerPos game + 5
       | otherwise = playerPos game
