module KeysController where

import Graphics.Gloss.Interface.Pure.Game
import Breakout

handleKeys :: Event -> BreakGame -> BreakGame
-- Interaction keys
handleKeys (EventKey (SpecialKey KeyEnter) Down   _ _) Start = initialState
handleKeys (EventKey (SpecialKey KeyEnter) Down   _ _) GameOver = initialState
-- Movement Keys
handleKeys (EventKey (Char 'a') Down _ _) game | game == Start || game == GameOver = game 
                                               | otherwise = game { leftLongMove   = True  }
handleKeys (EventKey (Char 'a') Up   _ _) game | game == Start || game == GameOver = game 
                                               | otherwise = game { leftLongMove   = False }
handleKeys (EventKey (Char 'd') Down _ _) game | game == Start || game == GameOver = game 
                                               | otherwise = game { rightLongMove  = True  }
handleKeys (EventKey (Char 'd') Up   _ _) game | game == Start || game == GameOver = game 
                                               | otherwise = game { rightLongMove  = False }
-- Special power test key
handleKeys (EventKey (SpecialKey KeySpace)  Down _ _) game | game == Start || game == GameOver = game 
                                                           | otherwise = game { isPaddleSticky = not (isPaddleSticky game)}
handleKeys (EventKey (SpecialKey KeySpace)  Up   _ _) game | game == Start || game == GameOver = game 
                                                           | otherwise = game
handleKeys _ game = game
