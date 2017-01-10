import Network.MateLight.Simple
import Network.MateLight

import SDLEventProvider.SDLKeyEventProvider

import Control.Monad.State
import Control.Monad.Reader

import Data.Maybe
import qualified Network.Socket as Sock

type KeyStatus = (String, String, Integer) -- Represents the tuple (KeyStatusString, KeyNameString, Time) 

move :: (Int, Int) -> KeyStatus -> (Int, Int) -> (Int, Int)
move (xdim, ydim) ("Pressed","w",_) (x,y) = (x, (y - 1) `mod` ydim)
move (xdim, ydim) ("Held","w",dur) (x,y) = if dur >= 100 then (x, (y - 1) `mod` ydim) else (x,y)
move (xdim, ydim) ("Pressed","a",_) (x,y) = ((x - 1) `mod` xdim, y)
move (xdim, ydim) ("Held","a",dur) (x,y) = if dur >= 100 then ((x - 1) `mod` xdim, y) else (x,y)
move (xdim, ydim) ("Pressed","s",_) (x,y) = (x, (y + 1) `mod` ydim)
move (xdim, ydim) ("Held","s",dur) (x,y) = if dur >= 100 then (x, (y + 1) `mod` ydim) else (x,y)
move (xdim, ydim) ("Pressed","d",_) (x,y) = ((x + 1) `mod` xdim, y)
move (xdim, ydim) ("Held","d",dur) (x,y) = if dur >= 100 then ((x + 1) `mod` xdim, y) else (x,y)
move _ _ (x,y) = (x,y)

toFrame :: (Int, Int) -> (Int, Int) -> ListFrame
toFrame (xdim, ydim) (x', y') = ListFrame $ map (\y -> map (\x -> if x == x' && y == y' then Pixel 0xff 0xff 0xff else Pixel 0 0 0) [0 .. xdim - 1]) [0 .. ydim - 1]

getKeyDataTuples keyState = (map (\(k,t) -> ("Pressed",k,t)) (pressed $ keyState)) ++ (map (\(k,d) -> ("Held",k,d)) (held $ keyState)) ++ (map (\(k,t) -> ("Released",k,t)) (released $ keyState))

eventTest :: [EventT] -> MateMonad ListFrame (Int,Int) IO ListFrame
eventTest events = do 
        state <- get
        let state' = foldl (\acc (EventT mod ev) -> if mod == "SDL_KEY_DATA" then foldl (\accState key -> move dim key accState) acc (getKeyDataTuples (read $ show ev)) else acc) state events
        put $ state'
        return (toFrame dim state')
  
dim :: (Int, Int)
dim = (30, 12)
  
main :: IO ()
main = do
    showSDLControlWindow
    Sock.withSocketsDo $ runMateM (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 33000) True [sdlKeyEventProvider]) eventTest (0,0)
