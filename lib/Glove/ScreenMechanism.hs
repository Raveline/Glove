module Glove.ScreenMechanism
where

import qualified Data.Vector as V
import Glove.Types

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


cleanGrid :: Console -> Console
cleanGrid c = let newGrid = getNewGrid . _config $ c in
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

unupdateAll :: Console -> Console
unupdateAll c@(Console _ g) = let newgrid = V.map (V.map unUpdate) g in
                                  c { _grid = newgrid }
    where unUpdate t = t { _updated = False }

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
