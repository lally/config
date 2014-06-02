{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Support.Launch where

import XMonad
import XMonad.Actions.TagWindows (getTags)
import XMonad.Core
import XMonad.Util.NamedWindows (getName)

import System.Posix.IO (createPipe)
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Support.Types
import IPC.Server

import Data.Set as S


-- | Monitor Thread.  At 10 Hz, look for changes in the XState and
-- update the ExtendedXState.  At 1 Hz, update the ExtendedXState. 
monitorThread ::
  TVar (Maybe XState) ->   -- ^ XState updated from xmonad
  TVar (ExtendedXState) -> -- ^ ExtendedXState maintained by monitorThread
  TVar ([X ()]) ->         -- ^ Action queue used to have things run in X monad
  IO ()
monitorThread stateTVar extendedStateTVar actionQueueTVar = 
  monitorThread' S.empty 1
  where
    queryWindow :: Window -> X ExtendedWindowInfo
    queryWindow win = do
      name <- getName win
      tags <- getTags win
      return ExtendedWindowInfo {
        wiTitle = (show name),
        wiTags = tags,
        wiWinId = win }
        
    updateExtendedState :: X ()
    updateExtendedState = do
      mxstate <- io $ atomically $ readTVar stateTVar
      exstate <- case mxstate of
                  Nothing -> return ExtendedXState { xsWindows = [] }
                  Just xstate ->  do
                    windowXStates <- mapM queryWindow $ S.elems $ mapped xstate
                    return ExtendedXState { xsWindows = windowXStates }
      io $ atomically $ writeTVar extendedStateTVar exstate

    monitorThread' :: (S.Set Window) -> Int -> IO ()
    monitorThread' !priorWindowSet !seqNo = do
      threadDelay 100000 -- 100 ms
      -- Update extended X state
      mcurState <- atomically $ do readTVar stateTVar
      let curWindowSet = maybe S.empty mapped mcurState
          setDirty = priorWindowSet /= curWindowSet
      if setDirty || (0 == (seqNo `mod` 10))
         then io $ atomically $ do
           actionQueue <- readTVar actionQueueTVar
           writeTVar actionQueueTVar $ actionQueue ++ [updateExtendedState]
         else return ()
      monitorThread' curWindowSet (1 + seqNo)

-- | Launches XMonad with the configuration given, an IPC server, and
-- a separate monitor thread.
launchXMonad :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
launchXMonad config = do
  actionQueueTVar <- newTVarIO []
  stateTVar <- newTVarIO Nothing
  xstateTVar <- newTVarIO ExtendedXState { xsWindows = [] }
  (readEnd, wrEnd) <- createPipe
  monitorThreadID <- forkIO $ monitorThread stateTVar xstateTVar actionQueueTVar
  ipcThreadID <- forkIO $ runIPCServer wrEnd xstateTVar actionQueueTVar
  xmonad readEnd actionQueueTVar stateTVar config
