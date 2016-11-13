{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Aria
import Data.Acid.Run
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Reader
import qualified Aria.Scripts as AS

serveConfig = AriaServerConfig 
  { _ariaServerAddress = "localhost"
  , _ariaServerPort = "3000"
  , _maxReceive = 2048
  }

main :: IO ()
main = withAcid (Just "/tmp/_state") defaultRepo $
  \acidState -> do
      siteState <- newTVarIO (RepoAppState acidState Nothing)
      flip runReaderT serveConfig $ serveAriaCommands siteState
