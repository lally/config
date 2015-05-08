module Support.GraphWin (windowMap, showWindows) where

import System.Process
import Data.List (intercalate)
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
  io $ putStrLn $ intercalate ", " $ map fst allWindows
