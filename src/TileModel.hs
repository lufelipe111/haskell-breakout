module TileModel where

import Graphics.Gloss

type Model = (Float, Float)

data Tile = Tile
  { tilePos    :: (Float, Float)
  , tileState  :: Integer
  , tileHeight :: Float
  , tileWidth  :: Float
  } deriving (Show, Eq)

data Tile' = Tile'
  { tilePicture'  :: Picture
  , tilePos'      :: (Float, Float)
  , tileState'    :: Integer
  , tileHeight'   :: Float
  , tileWidth'    :: Float
  } deriving (Show, Eq)

tilePic :: [Picture] -> Tile -> Picture
tilePic tileBMPs tile
  | state >= 1 = translate tileX tileY $ scale (width/bmpWidth) (height/bmpHeight) (tileBMPs !! fromIntegral state)
  | otherwise  = translate tileX tileY $ scale (width/bmpWidth) (height/bmpHeight) (tileBMPs !! fromIntegral state)
  where
    state  = tileState tile
    height = tileHeight tile
    width  = tileWidth tile
    bmpWidth = 384                       -- Largura do arquivo bitmap
    bmpHeight = 128                      -- Altura do arquivo bitmap
    (tileX, tileY)  = tilePos tile
