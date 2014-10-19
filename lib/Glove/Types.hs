-- Types used in gloves, mostly definition for Tiles
module Glove.Types
where

-- |Classical color representation with RGBA.
data Color = Color { _red :: Int
                   , _green :: Int
                   , _blue :: Int
                   , _alpha :: Int } deriving (Eq, Show)

black :: Color
black = Color 0 0 0 0
white :: Color
white = Color 255 255 255 0
red :: Color
red = Color 255 0 0 0
green :: Color
green = Color 0 255 0 0
blue :: Color
blue = Color 0 0 255 0

-- |A tile is a part of the screen. It carries
-- information about the current character displayed
-- at this position, the color of the background and
-- the foreground, an a flag to know if it should be
-- re-rendered or not.
data Tile = Tile { _character :: Char
                 , _backgroundColor :: BackColor
                 , _foregroundColor :: ForeColor
                 , _updated :: Bool } deriving (Eq, Show)

-- |The notion of width (can be expressed in character or pixel)
type Width = Int
-- |The notion of height (can be expressed in character or pixel)
type Height = Int
type Position = (Int, Int)
type ForeColor = Color
type BackColor = Color

-- |Given the used Font and the screen size in char,
-- get the resolution needed for the screen.
-- >>> computeRealScreenRes (Font "" 16 16) (Screen 80 20)
-- (1280,320)
-- >>> computeRealScreenRes (Font "" 12 10) (Screen 40 20)
-- (480,200)
computeRealScreenRes :: Font -> Screen -> Resolution
computeRealScreenRes f s = (computeRealScreenWidth f s
                           ,computeRealScreenHeight f s)
    where
        computeRealScreenWidth :: Font -> Screen -> Width
        computeRealScreenWidth f' s' = _sizeW f' * _charWidth s'
        computeRealScreenHeight :: Font -> Screen -> Height
        computeRealScreenHeight f' s' = _sizeH f' * _charHeight s'
