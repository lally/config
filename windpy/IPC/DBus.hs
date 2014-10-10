{-# LANGUAGE OverloadedStrings #-}

module IPC.DBus where

import DBus
import DBus.Client

{-
This works:
dbus-send --session --print-reply
          --reply-timeout=120000 --type=method_call
          --dest=$(serverBusName) $(objectPath)
          $(serverInterfaceName).$(dummyName)
-}

objectPath = "/org/xmonad/super" :: ObjectPath
serverInterfaceName = "org.xmonad.Super" :: InterfaceName
serverBusName = "org.xmonad.Super" :: BusName
dummyName = "Dummy"

dummyCommand :: IO String
dummyCommand = do
  putStrLn $ "dummyCommand!"
  return "Foo!"
  -- return $ replyReturn []

runIPCServer = do
  client <- connectSession
  nameRequest <- requestName client serverBusName []
  if nameRequest /= NamePrimaryOwner
     then do
       putStrLn "Failed to register name"
       return client
     else do 
       putStrLn ("Exporting " ++ show objectPath)
       export client objectPath [
         autoMethod serverInterfaceName dummyName dummyCommand ]
       return client

shutdownIPCServer client = do
  unexport client objectPath
  releaseName client serverBusName
