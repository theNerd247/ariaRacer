{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Aria.Repo
  ( newRacer
  , deleteRacer
  , buildRacer
  , getScriptLogs
  , uploadCode
  , selectBuild
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
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Time (getCurrentTime)
import Data.Acid
import Data.Acid.Advanced
import Data.Data
import Data.Text (Text)
import Data.Monoid ((<>))
import System.FilePath ((</>))
import Data.Char (isSpace)
import qualified Aria.Scripts as AS
import qualified Data.List as DL

type RepoAppState = AcidState RepoDBState

type RepoApp = StateT RepoAppState

data ScriptError =
  ScriptError AS.ScriptLog
  deriving (Read, Show, Ord, Eq, Data, Typeable)

instance Exception ScriptError

newRacer
  :: (Monad m, MonadCatch m, MonadIO m, MonadThrow m)
  => Racer -> RepoApp m RacerId
newRacer racer =
  handleAll (\_ -> undoNewUser >> (return $ RacerId 0)) $
  do acid <- get
     rid <- update' acid (InsertRacer racer)
     runScript (AS.CreateRacer rid)
     return rid

undoNewUser
  :: (Monad m, MonadIO m, MonadThrow m)
  => RepoApp m ()
undoNewUser = do
  acid <- get
  (RacerId rid) <- query' acid (GetNextRacerId)
  update' acid (RemoveRacer . RacerId $ rid - 1)

deleteRacer
  :: (MonadIO m, MonadThrow m)
  => RacerId -> RepoApp m ()
deleteRacer rid = do
  acid <- get
  update' acid (RemoveRacer rid)
  runScript (AS.RemoveRacer rid)
  return ()

buildRacer
  :: (MonadIO m, MonadThrow m)
  => RacerId -> SHA -> RepoApp m ()
buildRacer rid rev = do
  acid <- get
  runScript (AS.BuildRacer rid rev)
  return ()

uploadCode
  :: (MonadIO m, MonadThrow m)
  => RacerId -> FilePath -> Text -> RepoApp m ()
uploadCode rid file bName = do
  acid <- get
  bPath <- AS._scriptCwd <$> query' acid (GetScriptConfig)
  let outFile = bPath ++ "/racer_" ++ (show $ _unRacerId rid) ++ "_commit.out"
  runScript (AS.UploadCode rid file bName outFile)
  bRev <- liftIO $ DL.takeWhile (not . isSpace) <$> readFile outFile
  dt <- liftIO $ getCurrentTime
  racer <- query' acid $ GetRacerById rid
  let newBuild =
        RacerBuild
        { _buildName = bName
        , _buildRev = bRev
        , _buildDate = dt
        }
  case racer of
    Nothing -> return ()
    Just r -> do
      update' acid . UpdateRacer $
        (r & selectedBuild .~ 0 & racerBuilds %~ (newBuild :))
      return ()

getScriptLogs
  :: (MonadIO m, Monad m)
  => RepoApp m AS.ScriptLog
getScriptLogs = get >>= \acid -> query' acid GetScriptLog

selectBuild :: (MonadIO m, Monad m) => RacerId -> SHA -> RepoApp m ()
selectBuild rid sha = do
  acid <- get
  mr <- query' acid (GetRacerById rid)
  case mr of
    Nothing -> return ()
    (Just racer) -> do
      update'
        acid
        (UpdateRacer $
         racer & selectedBuild .~ setSelBuild (racer ^. racerBuilds))
      return ()
  where
    setSelBuild = maybe 0 toInteger . DL.findIndex ((== sha) . _buildRev)

-- | Run the given script command. Upon an ExitFailure throw a ScriptError exception
runScript
  :: (MonadIO m, MonadThrow m)
  => AS.ScriptCommand -> RepoApp m String
runScript cmd = do
  acid <- get
  config <- query' acid GetScriptConfig
  ((out, ecode), log) <- AS.runScriptCommand config cmd
  update' acid (AddScriptLog log)
  case ecode of
    0 -> return out
    c -> throwM $ ScriptError log
