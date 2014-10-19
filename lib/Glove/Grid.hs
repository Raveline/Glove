-- A default implementation for the free monad ScreenAction.
module Glove.Grid
( Console           -- The default Console implementation of Glove
 ,ConsoleConfig     -- The default console configuration
 ,Grid              -- A default grid implementation using Data.Vector
 ,ConsoleState      -- A default state for the interpreter
 ,initConsole       -- Prepare a console to be modified through run
 ,run               -- The default interpreter for the ScreenAction
 ,(!!!)             -- Given a console and a position, read the tile
)
where

import Control.Monad.Trans.Free
import Control.Monad.State
import qualified Data.Vector as V

import Glove.Types
import Glove.Screen

data Console = Console { _config :: ConsoleConfig
                       , _grid :: Grid }

data ConsoleConfig = ConsoleConfig { _defaultBack :: BackColor
                                   , _defaultFore :: ForeColor
                                   , _defaultChar :: Char
                                   , _width :: Width
                                   , _height :: Height }

-- |A 2D vector of Tiles
type Grid = V.Vector Tile
-- |A state allowing us to compose console modifying functions
type ConsoleState a = State Console a

-- | Transform a 2D position into a 1D index
($>|) :: Position -> Width -> Int
(x,y) $>| w = (y*w) + x

(!!!) :: Console -> Position -> Tile
(Console c g) !!! p = let index = p $>| _width c in
                          g V.! index

-- |Given default values, create a console with an initialized
-- grid using the defaults values.
initConsole :: BackColor -> ForeColor -> Char -> Width -> Height -> Console
initConsole b f c w h = let config = ConsoleConfig b f c w h
                            defGrid = getNewGrid config in
                            Console config defGrid

-- |Default interpreter. Uses a State Monad and the Data.Vector
-- library to emulate the 2D grid.
run :: ScreenAction (State Console) next -> ConsoleState next
run act = runFreeT act >>= run'

run' :: FreeF ScreenActionF next (ScreenAction (State Console) next) -> ConsoleState next
run' (Pure n)                       = return n
run' (Free (ClearScreen n))         = get >>= put . cleanGrid >> run n
run' (Free (ColorizeFront p c n))   = get >>= put . colorize (colorizeF c) p >> run n
run' (Free (ColorizeBack p c n))    = get >>= put . colorize (colorizeB c) p >> run n
run' (Free (PutAt p c n))           = get >>= put . setChar c p >> run n
run' (Free (ReadAt p n))            = do c <- get 
                                         let t = c !!! p 
                                         run (n t)
run' (Free (Rendered n))            = get >>= put . unupdateAll >> run n
run' (Free (SetAt p t n))           = get >>= put . setTile t p >> run n

-- |- Map a function f on the element in a 2D vector
--    identified by its position p
mapOn :: Position                       -- A position
         -> (Tile -> Tile)              -- A function
         -> Console
         -> Grid
mapOn p f (Console c g) = let index =  p $>| _width c
                              mapped = f (g V.! index) in
                          g V.// [(index, mapped)]

setOn :: Position -> Tile -> Console -> Grid
setOn p t = mapOn p (const t)

colorize :: (Tile -> Tile) -> Position -> Console -> Console
colorize f p c = let newGrid = mapOn p f c in
                     c { _grid = newGrid }

cleanGrid :: Console -> Console
cleanGrid c = let newGrid = getNewGrid . _config $ c in
                  c { _grid = newGrid }
setChar :: Char -> Position -> Console -> Console
setChar ch p c = let newGrid = mapOn p repChar c in
                    c { _grid = newGrid }
                where repChar t = t { _character = ch }

setTile :: Tile -> Position -> Console -> Console
setTile t p c = let newGrid = setOn p t c in
                    c { _grid = newGrid }

colorizeF :: ForeColor -> Tile -> Tile
colorizeF f t = t {_foregroundColor = f, _updated = True }

colorizeB :: BackColor -> Tile -> Tile
colorizeB b t = t {_backgroundColor = b, _updated = True }

unupdateAll :: Console -> Console
unupdateAll c@(Console _ g) = let newgrid = V.map unUpdate g in
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
getNewGrid (ConsoleConfig b f c w h) = V.replicate (h*w) (Tile c b f True)
