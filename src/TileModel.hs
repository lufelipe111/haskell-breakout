module TileModel where

import Graphics.Gloss
import System.IO

type Model = (Float, Float)

data Tile = Tile
  { tileColor  :: Color
  , tilePos    :: (Float, Float)
  , tileState  :: Integer
  , tileHeight :: Float
  , tileWidth  :: Float
  } deriving Show

data Tile' = Tile'
  { tilePicture'  :: Picture
  , tilePos'      :: (Float, Float)
  , tileState'    :: Integer
  , tileHeight'   :: Float
  , tileWidth'    :: Float
  } deriving Show

-- tilePic' :: Tile -> Picture
-- tilePic' tile
--   | state >= 1 = translate tileX tileY
--                $ color tileC
--                $ Polygon [ (0, 0)
--                          , (width, 0)
--                          , (width, height)
--                          , (0, height)
--                          , (0,0)
--                          ]
--   | otherwise = translate tileX tileY
--               $ color tileC
--               $ Polygon [ (width/2, height/2)
--                         , (2*width/3, 0)
--                         , (width, 0)
--                         , (width, height)
--                         , (0, height)
--                         , (0,0)
--                         , (width/3, 0)
--                         , (0, 0)
--                         ]
--   where
--     state  = tileState tile
--     height = tileHeight tile
--     width  = tileWidth tile
--     tileC  = tileColor tile
--     (tileX, tileY)  = tilePos tile

tilePic :: [Picture] -> Tile -> Picture
tilePic tileBMPs tile
  | state >= 1 = translate tileX tileY $ scale (width/bmpWidth) (height/bmpHeight) (tileBMPs !! fromIntegral state)
  | otherwise  = translate tileX tileY $ scale (width/bmpWidth) (height/bmpHeight) (tileBMPs !! fromIntegral state)
  where
    state  = tileState tile
    height = tileHeight tile
    width  = tileWidth tile
    tileC  = tileColor tile
    bmpWidth = 384
    bmpHeight = 128
    (tileX, tileY)  = tilePos tile

-- tilesBMP :: Tile -> Picture
-- tilesBMP tile 
--   | tileC == blue && state >= 1 = do t <- loadBMP "../assets/01-Breakout-Tiles.bmp"
--   | otherwise = loadBMP "../assets/03-Breakout-tiles.bmp"
--   where
--     state  = tileState tile
--     height = tileHeight tile
--     width  = tileWidth tile
--     tileC  = tileColor tile
--     (tileX, tileY)  = tilePos tile

