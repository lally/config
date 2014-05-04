module Support.WinGraph where

import Data.String.Utils (join)
import System.Process
import XMonad
import XMonad.StackSet as W
import XMonad.Util.NamedWindows (getName)

-- pulled straight from GridSelect sources.
windowMap :: X [(String,Window)]
windowMap = do
    ws <- gets windowset
    wins <- mapM keyValuePair (W.allWindows ws)
    return wins
 where keyValuePair w = flip (,) w `fmap` decorateName' w

decorateName' :: Window -> X String
decorateName' w = do
  fmap show $ getName w
 

showWindows :: X ()
showWindows = do
  allWindows <- windowMap
  io $ putStrLn $ join ", " $ map fst allWindows
  
  
              
