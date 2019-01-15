{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Client.Demo where

import BasicPrelude

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import Data.Aeson.Parser (json)

import Fco.Backend (setupBackend)
import Fco.Client
import Fco.Client.Pocket (setupPocket)
import Fco.Core.Config (setupConfig)
import Fco.Core.Console (setupConsole)
import Fco.Core.Struct
import Fco.Core.Types


-- application

run :: IO ()
run = do
    backend <- initializeBackend host port initRemoteTable
    node <- newLocalNode backend
    runProcess node $ do
      self <- getSelfPid
      configSrv <- setupConfig self
      conW <- setupConsole self
      pocketSrv <- setupPocket self config
      backendSrv <- setupBackend self config
      loop conW pocketSrv backendSrv
      where 
        loop conW client backend = do
          continue <- receiveWait [
                match $ handleQuit backendSrv, 
                match $ handlePocketResponse conW backendSrv, 
                match $ handleBackendResponse conW, 
                match $ handleConInput pocketSrv backendSrv]
          case continue of
            True -> loop  conW client backend 
            _ -> return ()
