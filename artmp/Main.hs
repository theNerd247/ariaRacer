{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Acid.Run
import Aria.Repo
import Aria.Repo.DB
import Aria.Types
import Control.Monad.State
import qualified Aria.Scripts as AS

testRacer :: Racer
testRacer =
  Racer
  { _racerName = "Bob"
  , _racerId = RacerId 7
  }

initRepo :: RepoDBState
initRepo =
  RepoDBState
  { _racerDB = emptyRacerDB
  , _nextRacerId = RacerId 1
  , _scriptLog = []
  , _scriptConfig =
    AS.ScriptConfig
    { AS._scriptBasePath = "/home/noah/src/com/ariaRacer/scripts"
    }
  }

main :: IO ()
main =
  withAcid (Just "/tmp/_state") initRepo $
  \acid -> do
    rid <- flip evalStateT acid $ newRacer testRacer
    putStrLn $ "Created Racer" ++ (show rid)
