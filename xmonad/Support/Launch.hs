{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Support.Launch where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Maybe
import System.IO (stderr)
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.Posix.IO (createPipe, fdWrite)
import System.Posix.Types
import XMonad
import XMonad.Actions.TagWindows (getTags, setTags)
import XMonad.Core
import XMonad.Util.NamedWindows (getName)

import Support.Types
import Support.Xextra
import IPC.Server

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Graphics.X11.Xlib.Extras as XE
import qualified Recognize as R

-- | Construct a WindowInfo for a window.  Mostly coddled together
-- from xprop(1) sources.
getWinInfo :: Display -> Window -> X WindowInfo
getWinInfo dpy win = do
  let propsForWindow atom = do
        textProp <- getTextProperty dpy win atom
        atomName <- getAtomName dpy atom
        if (isNothing atomName)
           then return []
           else textPropertyToStringList textProp
  winPropertyAtoms <- io $ listProperties dpy win
  winPropertyNames <- io $ getAtomNames dpy winPropertyAtoms
  winPropertyValues <- io $ mapM propsForWindow winPropertyAtoms
  return $ M.fromList $ zip winPropertyNames winPropertyValues

-- | Construct a ExtendedWindowInfo for a window.  Any prior wiAppTags
-- will have to get added back in.
queryWindow :: Display -> R.MatcherSet -> Window -> X ExtendedWindowInfo
queryWindow dpy matchers win = do
  name <- getName win
  tags <- getTags win
  -- Construct a WindowInfo and match tags against it.
  winInfo <- getWinInfo dpy win
  matchedTags <- io $ R.matchTags matchers winInfo
  return ExtendedWindowInfo {
    wiTitle = (show name),
    wiUserTags = tags,
    wiDynamicTags = matchedTags,
    wiAppTags = [],  -- TODO: load any previous app tags from the prior
                     -- state
    wiWinId = win }

-- | Monitor Thread.  At 10 Hz, look for changes in the XState and
-- update the ExtendedXState.  At 1 Hz, update the ExtendedXState. 
monitorThread ::
  TState ->        -- ^ XState updated from xmonad
  TXState ->       -- ^ ExtendedXState maintained by monitorThread
  Fd ->            -- ^ Notify pipe for the action queue.
  TActions ->      -- ^ Action queue used to have things run in X monad
  R.MatcherSet ->  -- ^ Tag Matcher configuration
  IO ()
monitorThread stateTVar extendedStateTVar fdNotify actionQueueTVar matchers = 
  monitorThread' S.empty 1
  where
    updateExtendedState :: Display -> X ()
    updateExtendedState dpy = do
      mxstate <- io $ atomically $ readTVar stateTVar
      exstate <- case mxstate of
                  Nothing -> return ExtendedXState { xsWindows = [] }
                  Just xstate ->  do
                    windowXStates <- mapM (queryWindow dpy matchers) $ S.elems $
                                     mapped xstate
                    return ExtendedXState { xsWindows = windowXStates }
      io $ infoM "monitorThread.updateExtendedState'" $
        ": ExtendedXState: " ++ (show exstate)
      io $ atomically $ writeTVar extendedStateTVar exstate

    monitorThread' :: (S.Set Window) -> Int -> IO ()
    monitorThread' !priorWindowSet !seqNo = do
      threadDelay 100000 -- 100 ms
      -- Update extended X state
      mcurState <- atomically $ do readTVar stateTVar
      let curWindowSet = maybe S.empty mapped mcurState
          setDirty = priorWindowSet /= curWindowSet
      if setDirty || (0 == (seqNo `mod` 10))
         then do io $ atomically $ do
                   actionQueue <- readTVar actionQueueTVar
                   writeTVar actionQueueTVar $
                     actionQueue ++ [updateExtendedState]
                 io $ infoM "monitorThread.monitorThread'" "Queuing an update"
                 io $ fdWrite fdNotify "M"
                 return ()
         else return ()
      monitorThread' curWindowSet (1 + seqNo)

-- | Launches XMonad with the configuration given, an IPC server, and
-- a separate monitor thread.  Also configures the root logger
launchXMonad :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
launchXMonad config = do
  -- Initialize logging
  stderrLogger <- streamHandler stderr WARNING
  let logFormat = "$prio\t$time\t$loggername:$msg"
  let formattedLogger =
        setFormatter stderrLogger $ simpleLogFormatter logFormat
  let rootLog = rootLoggerName
  updateGlobalLogger rootLog (setLevel WARNING)
  updateGlobalLogger rootLog (addHandler stderrLogger)

  actionQueueTVar <- newTVarIO [] :: IO (TActions)
  stateTVar <- newTVarIO Nothing :: IO (TState)
  xstateTVar <- newTVarIO ExtendedXState { xsWindows = [] } :: IO (TXState)
  (readEnd, wrEnd) <- createPipe
  matcherSet <- R.loadConfig
  monitorThreadID <-
    forkIO $
    monitorThread stateTVar xstateTVar wrEnd actionQueueTVar matcherSet
  ipcThreadID <- forkIO $ runIPCServer wrEnd xstateTVar actionQueueTVar
  xmonad readEnd actionQueueTVar stateTVar config
