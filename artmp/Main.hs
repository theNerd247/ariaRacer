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

mkNewRacer :: AriaServerApp IO (Maybe RaceHistoryData)
mkNewRacer = do 
  runAriaCommand $ SetupRaceCmd [RacerId 1, RacerId 1]
  runAriaCommand $ GetCurrentRaceDataCmd

main :: IO ()
main = flip runReaderT defaultAriaServerConfig $ liftIO . putStrLn . show =<< mkNewRacer
