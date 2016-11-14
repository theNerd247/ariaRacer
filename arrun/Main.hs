{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Aria
import Data.Acid.Run
import Data.Acid.Remote
import Network (PortNumber, PortID(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Reader
import qualified Aria.Scripts as AS

main :: IO ()
main = withAcid (Just "/tmp/_state") defaultRepo $
  \acidState -> do
      siteState <- newTVarIO (RepoAppState acidState Nothing)
      forkIO $ acidServer skipAuthenticationCheck (PortNumber (3001 :: PortNumber)) acidState
      flip runReaderT defaultAriaServerConfig $ serveAriaCommands siteState
