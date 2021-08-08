module KeysController where

import Graphics.Gloss.Interface.Pure.Game
import Breakout

handleKeys :: Event -> BreakGame -> BreakGame
handleKeys (EventKey (Char 'a') Down _ _) game = game { leftLongMove  = True  }
handleKeys (EventKey (Char 'a') Up   _ _) game = game { leftLongMove  = False }
handleKeys (EventKey (Char 'd') Down _ _) game = game { rightLongMove = True  }
handleKeys (EventKey (Char 'd') Up   _ _) game = game { rightLongMove = False }
handleKeys _ game = game


