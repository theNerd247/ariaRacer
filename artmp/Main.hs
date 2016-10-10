{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Acid.Run
import Aria.Repo
import Aria.Repo.DB
import Aria.Types
import Control.Monad.State

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
  , _dbConfig =
    RepoConfig
    { _baseRepoPath = "/tmp/tstRepos"
    }
  }

main :: IO ()
main =
  withAcid (Just "/tmp/_state") initRepo $
  \acid -> do 
    rid <- flip evalStateT acid $ newRacer testRacer
    putStrLn $ "Created Racer" ++ (show rid)
