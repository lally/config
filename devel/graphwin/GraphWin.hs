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
import Debug.Trace as T

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

--
-- Graphical Layout
--

-- policy functions
manyWindows = 8 :: Int
colWidth :: PreRenderSet -> Int
colWidth renderSet =
  if (length $ windows renderSet) > manyWindows
     then 150
     else 200
colHeight = 50 :: Int
tagWidth = 100 :: Int
tagHeight = 20 :: Int

-- draw preferences
fontSize = 10
fontFamily = "PragmataPro"

tagHeaders :: PreRenderSet -> Render [PangoLayout]
tagHeaders renderSet = do
  mapM makeTagLayout tags
  where
    tags = allTags renderSet
    makeTagLayout tag = do
      let len = length tag
      layout <- createLayout tag
      liftIO $ do
        layoutSetWidth layout $ Just (fromIntegral 100)
        layoutSetEllipsize layout EllipsizeMiddle
        layoutSetAlignment layout AlignRight
        layoutSetAttributes layout [
          AttrSize 0 len fontSize,
          AttrFamily 0 len fontFamily
          ]
      return layout

tagColumnHeaders :: PreRenderSet -> Render [PangoLayout]
tagColumnHeaders renderSet = do
  mapM makeColumnHeader wins
  where
    wins = windows renderSet
    ellipseMode = if length wins > manyWindows
                     then EllipsizeMiddle
                     else EllipsizeNone
    makeColumnHeader win = do
      let len = length $ displayTitle win
      layout <- createLayout $ displayTitle win
      liftIO $ do
        -- TODO(lally): Consider changing parameters (e.g., ellipsize)
        -- when we have many windows and/or a particularly long name.
        
        -- TODO(lally): Do a two-pass version.  Get the height in the
        -- first pass, and if it's more than 2 lines, truncate.

        -- TODO(lally): Put out [(PangoLayout, width)], so we can support
        -- heterogenous widths at higher levels.
        
        -- Really, this logic will need some later specialization to get
        -- just right.
        
        layoutSetWidth layout $ Just (fromIntegral $ colWidth renderSet)
        layoutSetAlignment layout AlignLeft
        layoutSetEllipsize layout ellipseMode
        layoutSetAttributes layout [
          AttrSize 0 len fontSize,
          AttrFamily 0 len fontFamily
          ]
      return layout

--
-- Drawing
--
updateCanvas :: PreRenderSet -> EventM EExpose Bool
updateCanvas renderSet = do
  win <- eventWindow
  liftIO $ do
  (width,height) <- drawableGetSize win
  -- Build a table of all the data from the pre-render set.
  let nrTags = length . allTags $ renderSet :: Int
  let white = Color 65535 65535 65535
  let topMargin = 15 :: Int
  -- Try rendering with cairo instead.
  renderWithDrawable win $ do
    setAntialias AntialiasGray
    setSourceColor white

    tagHeads <- tagHeaders renderSet
    colHeads <- tagColumnHeaders renderSet

    let colXIncr = 5 + colWidth renderSet :: Int
    let numCols = (width - 10 - tagWidth) `div` colXIncr
    let topOfRowHeads = topMargin + colHeight :: Int
    let bottomOfRowHeads = topOfRowHeads + (
          (5 + tagHeight) * (length tagHeads)) :: Int
    -- use Int coordinates
    let relMoveToI x y = relMoveTo (fromIntegral x) (fromIntegral y)
    let moveToI x y = moveTo (fromIntegral x) (fromIntegral y)
    let trace s = liftIO $ T.traceIO s
    
    trace ("Got " ++ show (length tagHeads) ++ " row headers")
    trace ("Got " ++ show (length colHeads) ++ " column headers")
    trace ("Can fit " ++ show numCols ++ " columns across.")
    trace (" width = " ++ show width ++ "\n" ++
           " tagWidth + 10 = " ++ show (10 + tagWidth) ++ "\n" ++
           " colXIncr = " ++ show colXIncr)
    trace ("Displaying " ++ show (length colHeads - numCols) ++ " on bottom row")
    
    moveToI 0 topMargin
    save
    -- draw the top column headers
    relMoveToI (10 + tagWidth) 0
    mapM (\lay -> do { showLayout lay; relMoveToI colXIncr 0; }) $ take numCols colHeads
    restore
    
    save
    -- draw the tag headers
    moveToI 0 topOfRowHeads
    mapM (\lay -> do { showLayout lay; relMoveToI 0 (5 + tagHeight); }) $ tagHeads
    restore

    save
    -- draw the bottom column headers
    moveToI (10 + tagWidth) bottomOfRowHeads
    mapM (\lay -> do { showLayout lay; relMoveToI colXIncr 0; }) $ drop numCols colHeads
  return True
  
main = do
  initGUI
  dia <- dialogNew
  allLines <- readFile "/home/lally/input.txt"
  let renderSet = readInput allLines
  dialogAddButton dia stockOk ResponseOk
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  -- Figure out how big the window should be
  let numWindows = length (windows renderSet)
  let numTags = length (allTags renderSet)
  let rawColWidth = numWindows * (5 + colWidth renderSet)
  let optimalWidth = tagWidth + 10 +
                     if numWindows > manyWindows
                        then rawColWidth `div` 2
                        else rawColWidth
  let reqWidth = min 900 optimalWidth
  let reqHeight = min 600 (2 * colHeight) + (tagHeight * numTags) + 15
  canvas `on` sizeRequest $ return (Requisition reqWidth reqHeight)
  -- Bind the layout
  canvas `on` exposeEvent $ updateCanvas renderSet
  -- Add 'canvas' to the dialog, by putting it under 'contain', the
  -- 'upper' part of the dialog.
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  return ()
