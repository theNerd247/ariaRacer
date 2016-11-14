{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Aria
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC

serveConfig =
  AriaServerConfig
  { _ariaServerAddress = "127.0.0.1"
  , _ariaServerPort = "3000"
  , _maxReceive = 2048
  }

mkNewRacer :: AriaServerApp IO RacerId
mkNewRacer = do 
  liftIO . putStrLn . BSC.unpack . encode $ NewRacerCmd "Bob"
  runAriaCommand $ NewRacerCmd "Jo"

main :: IO ()
main = flip runReaderT serveConfig $ do 
    rid <- mkNewRacer
    runAriaCommand $ DelRacerCmd rid
    rid2 <- mkNewRacer
    sha1 <- runAriaCommand $ UploadCodeCmd rid2 "/home/noah/src/temp/tst.cpp" "TestBuild1"
    runAriaCommand $ UploadCodeCmd rid2 "/home/noah/src/temp/cpp/src/main.cpp" "TestBuild2"
    runAriaCommand $ SelectBuildCmd rid2 sha1
