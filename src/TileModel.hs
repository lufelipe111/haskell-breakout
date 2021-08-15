module TileModel where

import Graphics.Gloss

type Model = (Float, Float)

data Tile = Tile
  { tilePos    :: (Float, Float)
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

tilePic :: [Picture] -> Tile -> Picture
tilePic tileBMPs tile
  | state >= 1 = translate tileX tileY $ scale (width/bmpWidth) (height/bmpHeight) (tileBMPs !! fromIntegral state)
  | otherwise  = translate tileX tileY $ scale (width/bmpWidth) (height/bmpHeight) (tileBMPs !! fromIntegral state)
  where
    state  = tileState tile
    height = tileHeight tile
    width  = tileWidth tile
    bmpWidth = 384
    bmpHeight = 128
    (tileX, tileY)  = tilePos tile
