module Glove.Types
where

import Control.Monad.State
import qualified Data.Vector as V

-- |Config information about the "font" that we will use,
-- namely a bitmap font. Path of the file and size of its
-- characters are needed to compute the size of the screen.
data Font = Font { _path :: FilePath
                 , _sizeW :: Width
                 , _sizeH :: Height }

-- |Config information about the logical screen, with a size
-- expressed in number of characters.
data Screen = Screen { _charWidth :: Width
                     , _charHeight :: Height }

-- |Classical color representation with RGBA.
data Color = Color { _red :: Int
                   , _green :: Int
                   , _blue :: Int
                   , _alpha :: Int } deriving (Eq, Show)

black = Color 0 0 0 0
white = Color 255 255 255 0
red = Color 255 0 0 0
green = Color 0 255 0 0
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

data Console = Console { _config :: ConsoleConfig
                       , _grid :: Grid }

data ConsoleConfig = ConsoleConfig { _defaultBack :: BackColor
                                   , _defaultFore :: ForeColor
                                   , _defaultChar :: Char
                                   , _width :: Width
                                   , _height :: Height }

-- |A 2D vector of Tiles
type Grid = V.Vector (V.Vector Tile)
-- |The notion of width (can be expressed in character or pixel)
type Width = Int
-- |The notion of height (can be expressed in character or pixel)
type Height = Int
type Position = (Int, Int)
-- |The real screen resolution (expressed in pixel).
type Resolution = (Width, Height)
-- |A state allowing us to compose console modifying functions
type ConsoleState a = State Console a
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
        computeRealScreenWidth f s = _sizeW f * _charWidth s
        computeRealScreenHeight :: Font -> Screen -> Height
        computeRealScreenHeight f s = _sizeH f * _charHeight s

-- |Given default values, create a console with an initialized
-- grid using the defaults values.
initConsole :: BackColor -> ForeColor -> Char -> Width -> Height -> Console
initConsole b f c w h = let config = ConsoleConfig b f c w h
                            defGrid = getNewGrid config in
                            Console config defGrid

cleanGrid :: Console -> Console
cleanGrid c = let newGrid = getNewGrid . _config $ c in
                  c { _grid = newGrid }

-- |Given a width and a size, a default background color,
-- a default foreground color, a default char, initialize
-- a grid filled with Tiles of the given colors and char.
-- >>> let c = getNewGrid (ConsoleConfig 20 20 black black ' ')
-- >>> c V.! 0 V.! 0 == Tile ' ' black black False
-- True
-- >>> c V.! 19 V.! 19 == Tile ' ' black black False
-- True
getNewGrid :: ConsoleConfig -> Grid
getNewGrid (ConsoleConfig b f c w h) = V.replicate h (V.fromList cols)
    where cols :: [Tile]
          cols = replicate w (Tile c b f True)

(!!!) :: V.Vector (V.Vector a) -> Position -> a
v !!! (x,y) = v V.! y V.! x

-- |- Map a function f on the element in a 2D vector
--    identified by its position p
mapOn :: Position                       -- A position
         -> (a -> a)                    -- A function
         -> V.Vector (V.Vector a)       -- A 2D vector
         -> V.Vector (V.Vector a)       -- The new vector
mapOn (x,y) f v = v V.// [(y, setSubV)]
    where subV = v V.! y
          setSubV = subV V.// [(x, f (subV V.! x))]

setOn :: Position -> a -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
setOn p t = mapOn p (const t)

colorize :: (Tile -> Tile) -> Position -> Console -> Console
colorize f p c = let newGrid = mapOn p f . _grid $ c in
                     c { _grid = newGrid }

setChar :: Char -> Position -> Console -> Console
setChar ch p c = let newGrid = mapOn p repChar . _grid $ c in
                    c { _grid = newGrid }
                where repChar t = t { _character = ch }

setTile :: Tile -> Position -> Console -> Console
setTile t p c = let newGrid = setOn p t . _grid $ c in
                    c { _grid = newGrid }

colorizeF :: ForeColor -> Tile -> Tile
colorizeF f t = t {_foregroundColor = f, _updated = True }

colorizeB :: BackColor -> Tile -> Tile
colorizeB b t = t {_backgroundColor = b, _updated = True }
