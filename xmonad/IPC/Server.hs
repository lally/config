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
    { commandChan :: TVar ([X ()])
    , stateChan :: TVar ExtendedXState
    , notifyPipe :: Fd }
    
mkYesod "IPCServer" [parseRoutes|
/     HomeR    GET
|]

instance Yesod IPCServer

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    -- Set the HTML <title> tag.
    setTitle "IPC for XMonad"
    server <- ask
    state <- liftIO $ atomically $ readTVar $ stateChan server
    -- Hamlet is the standard HTML templating language used by Yesod.
    -- In this case, we include some specific markup to take advantage of
    -- the bootstrap CSS we just included.
    -- For more information on Hamlet, please see:
    -- http://www.yesodweb.com/book/shakespearean-templates
    [whamlet|
        <h1>XMonad
    |]

runIPCServer :: Fd -> TVar ExtendedXState -> TVar ([X ()]) -> IO ()
runIPCServer notifyPipe inState outCmd = warp 3000 (IPCServer outCmd inState notifyPipe)
