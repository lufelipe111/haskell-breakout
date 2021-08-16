module PaddleController where

import Breakout

movePaddle :: BreakGame -> BreakGame
movePaddle game = game'
  where
    leftBorder = playerPos game - (pWidth / 2)
    rightBorder = playerPos game + (pWidth / 2)
    x' | leftLongMove game  
         && not (rightLongMove game) 
         && leftBorder > fromIntegral (-width `div` 2) = playerPos game - playerVel game
       | rightLongMove game 
         && not (leftLongMove  game)
         && rightBorder < fromIntegral (width `div` 2) = playerPos game + playerVel game
       | otherwise = playerPos game
    game' | game == GameOver = game
          | otherwise = game { playerPos = x' }
