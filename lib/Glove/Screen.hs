module Glove.Screen
( ScreenAction,
  initConsole,
  clearScreen,
  colorizeFront,
  colorizeBack,
  putAt,
  readAt,
  rendered,
  setAt,
  run )
where

import Control.Monad.State
import Control.Monad.Trans.Free

import Glove.Types
import Glove.ScreenMechanism

-- UTILITIES
---------------------

-- |Given default values, create a console with an initialized
-- grid using the defaults values.
initConsole :: BackColor -> ForeColor -> Char -> Width -> Height -> Console
initConsole b f c w h = let config = ConsoleConfig b f c w h
                            defGrid = getNewGrid config in
                            Console config defGrid

-- SCREEN INTERACTION
---------------------

data ScreenActionF next 
    = ClearScreen next
    | ColorizeFront Position Color next
    | ColorizeBack Position Color next
    | PutAt Position Char next
    | ReadAt Position (Tile -> next)
    | Rendered next
    | SetAt Position Tile next

instance Functor ScreenActionF where
    fmap f (ClearScreen n) = ClearScreen (f n)
    fmap f (ColorizeFront p c n) = ColorizeFront p c (f n)
    fmap f (ColorizeBack p c n) = ColorizeBack p c (f n)
    fmap f (PutAt p c n) = PutAt p c (f n)
    fmap f (ReadAt p n) = ReadAt p (f . n)
    fmap f (Rendered n) = Rendered (f n)
    fmap f (SetAt p t n) = SetAt p t (f n)

-- |Screen interactions type. Use a free monad transformer.
-- A basic interpreter is provided (run), but you can roll
-- your own if you have your own screengrid implementation.
type ScreenAction = FreeT ScreenActionF

-- |Put every tile in the default configuration of the console.
clearScreen :: (Monad m) => ScreenAction m ()
clearScreen = liftF $ ClearScreen ()

-- |Change the front color of a tile.
colorizeFront :: (Monad m) => Position -> Color -> ScreenAction m ()
colorizeFront p c = liftF $ ColorizeFront p c ()

-- |Change the back color of a tile.
colorizeBack :: (Monad m) => Position -> Color -> ScreenAction m ()
colorizeBack p c = liftF $ ColorizeBack p c ()

-- |Put a character in a tile
putAt :: (Monad m) => Position -> Char -> ScreenAction m ()
putAt p c = liftF $ PutAt p c ()

-- |Get the values of a tile
readAt :: (Monad m) => Position -> ScreenAction m Tile
readAt p = liftF $ ReadAt p id

-- |Must be called after game has been rendered.
-- Set every tile "updated" flag as False
rendered :: (Monad m) => ScreenAction m ()
rendered = liftF $ Rendered ()

-- |Set every information a given tile.
setAt :: (Monad m) => Position -> Tile -> ScreenAction m ()
setAt p t = liftF $ SetAt p t ()

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
                                         let t = _grid c !!! p 
                                         run (n t)
run' (Free (Rendered n))            = get >>= put . unupdateAll >> run n
run' (Free (SetAt p t n))           = get >>= put . setTile t p >> run n
