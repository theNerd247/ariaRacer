{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Aria.Repo where

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

type RepoApp m a = (MonadThrow m, MonadIO m) =>
                   StateT RepoAppState m a

data ScriptError =
  ScriptError AS.ScriptCommand
              AS.ReturnCode
  deriving (Read, Show, Ord, Eq, Data, Typeable)

instance Exception ScriptError

newRacer :: Racer -> RepoApp m RacerId
newRacer racer = do
  acid <- get
  rid <- update' acid (UpsertRacer racer)
  runScript (AS.CreateRacer rid)
  return rid

deleteRacer :: RacerId -> RepoApp m ()
deleteRacer rid = do
  acid <- get
  update' acid (RemoveRacer rid)
  runScript (AS.RemoveRacer rid)

buildRacer :: RacerId -> CodeRevision -> RepoApp m ()
buildRacer rid rev = do
  acid <- get
  runScript (AS.BuildRacer rid rev)

runScript :: AS.ScriptCommand-> RepoApp m ()
runScript cmd = do
  acid <- get
  config <- query' acid GetScriptConfig
  (out, log) <- AS.runScriptCommand config cmd
  update' acid (AddScriptLog log)
  case out of
    0 -> return ()
    c -> throwM $ ScriptError cmd c
