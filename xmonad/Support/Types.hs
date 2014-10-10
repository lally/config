module Support.Types
    ( ExtendedWindowInfo(..)
    , ExtendedXState(..)
    , Tag(..), wiTags
    , WindowInfo(..)
    , Action, TActions, TXState, TState
    ) where

import Control.Concurrent.STM.TVar
import Data.List (nub)
import qualified Data.Map.Strict as M
import Graphics.X11.Types (Window)
import XMonad.Core

-- | We'll add metadata later.  Note that this will require that we
-- find a different store for the tags than the current value -- the X
-- properties attached to the window.
data Tag = Tag String
         deriving Eq
                  
instance Show Tag where
  show (Tag str) = str

-- | Map of X Window properties to matchers
type WindowInfo = M.Map String [String]

-- | Repository for all information known about each window. The
-- monitor thread maintains this structure.  It updates it
-- periodically.
data ExtendedWindowInfo = ExtendedWindowInfo
    { wiTitle :: String       -- ^The Text Title of the Window
    , wiUserTags :: [String]  -- ^String identifiers for the tags the
                              -- user applied through the TagWindows
                              -- extension
    , wiDynamicTags :: [Tag]  -- ^Dynamically matched tags from user rules.
    , wiAppTags :: [Tag]      -- ^Program-added tags 
    , wiWinId :: Window       -- ^Raw window identifier.
    } deriving Show

wiTags winInfo = nub $ (wiUserTags winInfo) ++
                 (map show $ (wiDynamicTags winInfo) ++ (wiAppTags winInfo))


-- | Repository for all information that we maintain in this window
-- manager.
data ExtendedXState = ExtendedXState
    { xsWindows :: [ExtendedWindowInfo] -- ^All windows
    } deriving Show

type TXState = TVar (ExtendedXState)
