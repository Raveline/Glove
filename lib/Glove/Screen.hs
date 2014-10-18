module Glove.Screen
where

import Control.Monad.State
import Control.Monad.Trans.Free
import Glove.Types

data ScreenActionF next 
    = ClearScreen next
    | ColorizeFront Position Color next
    | ColorizeBack Position Color next
    | Initialize BackColor ForeColor Char Width Height next
    | PutAt Position Char next
    | ReadAt Position (Tile -> next)
    | SetAt Position Tile next

instance Functor ScreenActionF where
    fmap f (Initialize b fo c w h n) = Initialize b fo c w h (f n)
    fmap f (ClearScreen n) = ClearScreen (f n)
    fmap f (ColorizeFront p c n) = ColorizeFront p c (f n)
    fmap f (ColorizeBack p c n) = ColorizeBack p c (f n)
    fmap f (PutAt p c n) = PutAt p c (f n)
    fmap f (ReadAt p n) = ReadAt p (f . n)
    fmap f (SetAt p t n) = SetAt p t (f n)

type ScreenAction = FreeT ScreenActionF

initialize :: (Monad m) => BackColor 
                        -> ForeColor 
                        -> Char 
                        -> Width 
                        -> Height 
                        -> ScreenAction m ()
initialize b f c w h = liftF $ Initialize b f c w h ()

clearScreen :: (Monad m) => ScreenAction m ()
clearScreen = liftF $ ClearScreen ()

colorizeFront :: (Monad m) => Position -> Color -> ScreenAction m ()
colorizeFront p c = liftF $ ColorizeFront p c ()

colorizeBack :: (Monad m) => Position -> Color -> ScreenAction m ()
colorizeBack p c = liftF $ ColorizeBack p c ()

putAt :: (Monad m) => Position -> Char -> ScreenAction m ()
putAt p c = liftF $ PutAt p c ()

readAt :: (Monad m) => Position -> ScreenAction m Tile
readAt p = liftF $ ReadAt p id

setAt :: (Monad m) => Position -> Tile -> ScreenAction m ()
setAt p t = liftF $ SetAt p t ()

run :: ScreenAction (State Console) next -> ConsoleState next
run act = runFreeT act >>= run'

run' :: FreeF ScreenActionF next (ScreenAction (State Console) next) -> ConsoleState next
run' (Free (Initialize b f c w h n))= put (initConsole b f c w h) >> run n
run' (Free (ClearScreen n))         = get >>= put . cleanGrid >> run n
run' (Free (ColorizeFront p c n))   = get >>= put . colorize (colorizeF c) p >> run n
run' (Free (ColorizeBack p c n))    = get >>= put . colorize (colorizeB c) p >> run n
run' (Free (PutAt p c n))           = get >>= put . setChar c p >> run n
run' (Free (ReadAt p n))            = do c <- get 
                                         let t = _grid c !!! p 
                                         run (n t)
run' (Free (SetAt p t n))           = get >>= put . setTile t p >> run n
