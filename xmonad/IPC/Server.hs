{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}

module IPC.Server (runIPCServer) where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.String
import System.IO
import System.Posix.Types
import XMonad
import Yesod
import Support.Types

{- Where we put in all the application state we care about.  This will
   probably soon include our view of the world (or just the TVar) -}
data IPCServer = IPCServer
    { commandChan :: TVar ([Display -> X ()])
    , stateChan :: TVar ExtendedXState
    , notifyPipe :: Fd }

mkYesod "IPCServer" [parseRoutes|
/     HomeR    GET
|]

instance Yesod IPCServer

getHomeR :: Handler TypedContent
getHomeR = do
  server <- ask
  state <- liftIO $ atomically $ readTVar $ stateChan server
  selectRep $ do
    provideRep $ return $ [shamlet|
       <html>
          <head>
             <title>IPC for XMonad
             <style>
               body: { font-face: "Ubuntu"; }
          <body>
             <h1>Super XMonad Windows
             <ul class="winlist_mapped">
                $forall winInfo <- xsWindows state
                   <li> <b>#{wiTitle winInfo}</b>
                        $if wiMapped winInfo
                            <it>MAPPED</it>
                        (#{show $ wiWinId winInfo}) -
                        #{show $ wiTags winInfo}
      |]
    provideRep $ return $ object [ "windows" .= [object
        [ "title" .= wiTitle winInfo, "id" .= wiWinId winInfo
        , "tags" .= wiTags winInfo] | winInfo <- xsWindows state ]]

runIPCServer :: Fd -> TVar ExtendedXState -> TVar ([Display -> X ()]) -> IO ()
runIPCServer notifyPipe inState outCmd = warp 3000 (
  IPCServer outCmd inState notifyPipe)
