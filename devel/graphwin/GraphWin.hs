{- |
Module      :  GraphWin.hs
Description :  Graphical Tag/Window Explorer
Copyright   :  (c) Lally Singh
License     :  All rights reserved.

Maintainer  :  yell@lal.ly
Stability   :  experimental
Portability :  non-portable (portability not tested)

Brings up an explorer of all windows on all desktops, mapped against the tags.
FUTURE TODO: Add matchers and inspectors to extract more information about the windows
 (e.g., pull all tab URLs from chrome, auto-classify the URLs against projects)
-}

-- Example of an drawing graphics onto a canvas. Note that this example
-- uses the old-style Gdk drawing functions. New implementations should
-- use Cairo. See examples in that directory.
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Types
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.GC
import Control.Monad.Trans ( liftIO )
import Data.String.Utils
import Data.List ( nub, lines )
import Data.Tuple ( uncurry, fst, snd )

-- Tags may get more complex later, as they go hierarchical, or aspect
-- related (e.g., tls-dev.research).   For now, strings

-- Windows can get more complex later as well.  Categorize:
--  A browser? Terminal? Popup? Emacs?
data TagWindow = TagWindow
    { displayTitle :: String -- ^ Window Title
    , tags :: [String] -- ^ Tags attached
    } deriving (Eq, Show)

data PreRenderSet = PreRenderSet
    { windows :: [TagWindow] -- ^ Tagged windows
    , allTags :: [String] -- ^ The full set of tags
    } deriving (Eq, Show)

parseWin line =
    TagWindow { displayTitle = title, tags = allTags }
    where
      majorFields = split "|" line
      title = head majorFields
      allTags = case length majorFields of
                  1 -> []
                  otherwise -> map strip $ split "," $ head $ tail majorFields
                  
readInput :: String -> PreRenderSet
readInput text =
    PreRenderSet { windows = allWindows, allTags = nub $ concatMap tags allWindows }
    where
      allWindows= map parseWin $ filter ((> 1) . length) $ lines text

updateCanvas :: PangoLayout -> EventM EExpose Bool
updateCanvas text = do
  win <- eventWindow
  liftIO $ do
  (width,height) <- drawableGetSize win
  -- Try rendering with cairo instead.
  renderWithDrawable win $ do
    setAntialias AntialiasGray
    setSourceColor $ Color 65535 0 0
    let points = map (\(a,b) -> (fromIntegral a, fromIntegral b)) [
                            (200,30),(width-30,height-30),(width-30,30),(30,height-30)]
    uncurry moveTo $ head points
    mapM_ (uncurry lineTo) $ tail points
    stroke
  return True
  
main = do
  initGUI
  dia <- dialogNew
  allLines <- readFile "/home/lally/input.txt"
  let elements = split "," allLines
  let windowNames = unlines elements
  dialogAddButton dia stockOk ResponseOk
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  canvas `on` sizeRequest $ return (Requisition 800 450)
  -- a Pango text layout.
  text <- canvas `widgetCreateLayout` windowNames
  -- - Bind the layout
  canvas `on` exposeEvent $ updateCanvas text
  -- add 'canvas' to the dialog, by putting it under 'contain', the 'upper' part of the dialog.
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  return ()

{-    
  gc <- gcNew win
  gcSetValues gc $ newGCValues {
    foreground = Color 65535 0 0,
    capStyle = CapRound,
    lineWidth  = 20,
    joinStyle = JoinRound
  }
  drawLines win gc [(30,30),(width-30,height-30),(width-30,30),(30,height-30)]
  gcSetValues gc $ newGCValues {
    foreground = Color 65535 65535 0,
    lineWidth = 4
  }
  drawArc win gc False 0 0 width height (135*64) (90*64)

  drawLayoutWithColors win gc 30 (height `div` 2) text
    (Just (Color 0 0 0)) Nothing
  return True
-}
