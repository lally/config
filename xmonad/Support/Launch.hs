{-# LANGUAGE FlexibleContexts, BangPatterns, DeriveDataTypeable #-}

module Support.Launch where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad.State.Strict

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
import qualified XMonad.Util.ExtensibleState as XS

import Support.Types
import Support.Xextra
import IPC.Server

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Graphics.X11.Xlib.Extras as XE
import qualified Recognize as R

data AtomCache = AtomCache { atomSeqno :: Int
                           , atomMap :: M.Map Atom (Int, String)
                           } deriving (Eq, Show, Typeable)

instance ExtensionClass AtomCache where
  initialValue = AtomCache 0 (M.fromList [])

-- Plan
-- ----

-- Implement an atom cache.  I'm looking for a way to determine which
-- atoms are no longer used, and to reuse the lookups I've already
-- done.  Actually, copy them to local strings and kill them
-- immediately.  Use the cache for performance.


wrappedAtomName :: Display -> Atom -> IO String
wrappedAtomName dpy atom = do
  atomName <- getAtomName dpy atom
  if (isNothing atomName)
    then return ""
    else return $ fromJust atomName

startLookups :: AtomCache -> AtomCache
startLookups prior = prior { atomSeqno = 1 + (atomSeqno prior) }

lookupAtom :: Display -> Atom -> X String
lookupAtom dpy atom = do
  cache <- XS.get
  let newSeqNo = atomSeqno cache
      lookupRes = M.lookup atom $ atomMap cache
      setKey k (n, s) = (newSeqNo, s)
      mapCache = atomMap cache

  str <- if isJust lookupRes
         then return $ snd $ fromJust lookupRes
         else liftIO $ wrappedAtomName dpy atom

  let updatedMap = if isJust lookupRes
                   then M.adjustWithKey setKey atom mapCache
                   else M.insert atom (newSeqNo, str) mapCache
  XS.put (cache { atomMap = updatedMap })
  return str

finishLookups :: AtomCache -> AtomCache
finishLookups prior =
  let threshold = atomSeqno prior
      filteredMap = M.filterWithKey (\k (seq,s) -> seq == threshold) $ atomMap prior
  in prior { atomMap = filteredMap }

-- | Construct a WindowInfo for a window.  Mostly coddled together
-- from xprop(1) sources.
getWinInfo :: Display -> Window -> X WindowInfo
getWinInfo dpy win = do
  -- MEMORY: Note: #3 in profiler for memory comes from textPropertyToStringList.
  let propsForWindow :: Atom -> X [String]
      propsForWindow atom = do
        cache <- get
        textProp <- liftIO $ getTextProperty dpy win atom
        atomName <- lookupAtom dpy atom
        result <- liftIO $ textPropertyToStringList textProp
        return result

  winPropertyAtoms <- io $ listProperties dpy win
  winPropertyNames <- mapM (lookupAtom dpy) winPropertyAtoms
  winPropertyValues  <- mapM propsForWindow winPropertyAtoms
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
    -- TODO: Lally: The action queue (TActions) needs to be StateT AtomCache X .., not just X ...
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
         then do atomically $ do
                   actionQueue <- readTVar actionQueueTVar
                   writeTVar actionQueueTVar $
                     actionQueue -- ++ [updateExtendedState]
                 infoM "monitorThread.monitorThread'" "Queuing an update"
                 fdWrite fdNotify "M"
                 return ()
         else return ()
      monitorThread' curWindowSet (1 + seqNo)

fetchAgent :: Fd -> Fd -> TActions -> TState -> Display -> XEventPtr -> X Event
-- fdNotify is a notify pipe.  Any data on there indicates
-- that we have data to consume from inCMD.
fetchAgent fdNotify fdDpy inCmd outState dpy e = do
  numQueuedEvents <- io $ pending dpy
  if numQueuedEvents <= 0
     then repeatedRead  -- refill the X event queue.
     else return ()
  io $ nextEvent dpy e
  event <- io $ getEvent e
  return event
  where
    repeatedRead :: X ()
    repeatedRead =
      do selectResult <- io $ select' [fdNotify, fdDpy] [] [] (
           ST.Time $ ST.CTimeval 10 0)
         xstate <- get
         io $ atomically $ do writeTVar outState $ Just xstate
         let readFds = sel1 $ fromMaybe ([],[],[]) selectResult
         if fdNotify `elem` readFds
            then do (_,_) <- io $ fdRead fdNotify 128
                    ops <- io $ atomically $ do ops' <- readTVar inCmd
                                                writeTVar inCmd []
                                                return ops'
                    io $ infoM "xmonad.repeatedRead" $
                      "xmonad: Running " ++ (show $ length ops) ++
                      " actions"
                    mapM (\f -> f dpy) ops
                    return ()
            else return ()
         -- Repeat as long as we don't have data to read from
         -- the display's FD
         if fdDpy `notElem` readFds
            then repeatedRead
            else return ()

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
  xmonadCustom (fetchAgent readEnd actionQueueTVar stateTVar) config
