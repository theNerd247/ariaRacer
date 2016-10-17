{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Aria.Repo
  ( newRacer
  , deleteRacer
  , buildRacer
  , getScriptLogs
  , AS.scriptBasePath
  , AS.scriptStartTime
  , AS.scriptEndTime
  , AS.scriptFile
  , AS.scriptArgs
  , AS.stdErr
  , AS.stdOut
  , AS.exitCode
  , AS.scriptCmd
  , RepoAppState(..)
  , RepoApp(..)
  , ScriptError(..)
  , RepoDB
  , RepoDBState(..)
  ) where

import Aria.Repo.DB
import Aria.Types
import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Acid.Advanced
import Data.Data
import Data.Monoid ((<>))
import System.FilePath ((</>))
import qualified Aria.Scripts as AS

type RepoAppState = AcidState RepoDBState

type RepoApp = StateT RepoAppState

data ScriptError =
  ScriptError AS.ScriptLog
  deriving (Read, Show, Ord, Eq, Data, Typeable)

instance Exception ScriptError

newRacer
  :: (MonadIO m, MonadThrow m)
  => Racer -> RepoApp m RacerId
newRacer racer = do
  acid <- get
  rid <- update' acid (InsertRacer racer)
  runScript (AS.CreateRacer rid)
  return rid

deleteRacer
  :: (MonadIO m, MonadThrow m)
  => RacerId -> RepoApp m ()
deleteRacer rid = do
  acid <- get
  update' acid (RemoveRacer rid)
  runScript (AS.RemoveRacer rid)

buildRacer
  :: (MonadIO m, MonadThrow m)
  => RacerId -> SHA -> RepoApp m ()
buildRacer rid rev = do
  acid <- get
  runScript (AS.BuildRacer rid rev)

getScriptLogs
  :: (MonadIO m, Monad m)
  => RepoApp m AS.ScriptLog
getScriptLogs = get >>= \acid -> query' acid GetScriptLog

-- | Run the given script command. Upon an ExitFailure throw a ScriptError exception
runScript
  :: (MonadIO m, MonadThrow m)
  => AS.ScriptCommand -> RepoApp m ()
runScript cmd = do
  acid <- get
  config <- query' acid GetScriptConfig
  (out, log) <- AS.runScriptCommand config cmd
  update' acid (AddScriptLog log)
  case out of
    0 -> return ()
    c -> throwM $ ScriptError log
