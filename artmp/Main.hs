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
  liftIO . putStrLn . BSC.unpack . encode $ NewRacerCmd newRacer
  runAriaCommand $ NewRacerCmd newRacer
  where 
    newRacer = Racer
      { _racerId = RacerId 0
      , _racerName = "Bob"
      , _selectedBuild = Nothing
      }

main :: IO ()
main = putStrLn . show =<< flip runReaderT serveConfig mkNewRacer
