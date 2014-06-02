module Support.Types
    ( ExtendedWindowInfo(..)
    , ExtendedXState(..)
    ) where

import Graphics.X11.Types (Window)

-- | Repository for all information known about each window. The
-- monitor thread maintains this structure.  It updates it
-- periodically.
data ExtendedWindowInfo = ExtendedWindowInfo
    { wiTitle :: String   -- ^The Text Title of the Window
    , wiTags :: [String]  -- ^String identifiers for the tags
    , wiWinId :: Window      -- ^Raw window identifier.
    }

-- | Repository for all information that we maintain in this window
-- manager.
data ExtendedXState = ExtendedXState
    { xsWindows :: [ExtendedWindowInfo] -- ^All windows
    }

