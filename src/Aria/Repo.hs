{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Aria.Repo
  ( newRacer
  , deleteRacer
  , getScriptLogs
  , uploadCode
  , selectBuild
  , getRacerAcid
  , getRacers
  , withRacer
  , defaultRepo
  , guardMaybe
  , AS.scriptBasePath
  , AS.scriptCwd
  , AS.scriptStartTime
  , AS.scriptEndTime
  , AS.scriptFile
  , AS.scriptArgs
  , AS.stdErr
  , AS.stdOut
  , AS.exitCode
  , RacerNotFound(..)
  , InvalidBuildError(..)
  , module Aria.Acid
  ) where

import Aria.Acid
import Aria.Types
import Aria.RaceHistory
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Time (UTCTime,getCurrentTime, diffUTCTime)
import Data.Acid
import Data.Acid.Advanced
import Data.Data
import Data.Text (Text)
import Data.Monoid ((<>))
import System.FilePath ((</>))
import Data.Char (isSpace)
import qualified Aria.Scripts as AS
import qualified Data.List as DL

data RacerNotFound =
  RacerNotFound RacerId
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Exception RacerNotFound

data InvalidBuildError = InvalidBuildError
  RacerId
  deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Exception InvalidBuildError

getRacerAcid :: (MonadReader RepoAcid m) => m RepoAcid
getRacerAcid = ask

newRacer
  :: (Monad m, MonadCatch m, MonadIO m, MonadThrow m, MonadReader RepoAcid m)
  => Racer -> m RacerId
newRacer racer = do 
     acid <- ask
     rid <- update' acid (InsertRacer racer)
     runScript (AS.CreateRacer rid)
     return rid 
  `catchAll`
  (\_ -> undoNewUser >> (return $ RacerId 0))

undoNewUser
  :: (Monad m, MonadIO m, MonadThrow m, MonadReader RepoAcid m)
  => m ()
undoNewUser = do
  acid <- ask
  (RacerId rid) <- query' acid (GetNextRacerId)
  update' acid (RemoveRacer . RacerId $ rid - 1)

deleteRacer
  :: (MonadIO m, MonadThrow m, MonadReader RepoAcid m)
  => RacerId -> m ()
deleteRacer rid = do
  acid <- ask
  update' acid (RemoveRacer rid)
  runScript (AS.RemoveRacer rid)
  return ()

uploadCode
  :: (MonadIO m, MonadThrow m, MonadReader RepoAcid m)
  => RacerId -> FilePath -> Text -> m SHA
uploadCode rid file bName =
  withRacer rid $
  \racer -> do
    acid <- ask
    bPath <- AS._scriptCwd <$> query' acid (GetScriptConfig)
    let outFile = bPath ++ "/racer_" ++ (show $ _unRacerId rid) ++ "_commit.out"
    runScripts $
      [ (AS.UploadCode rid file)
      , (AS.BuildRacer rid "")
      , (AS.CommitBuild rid bName outFile)
      ]
    bRev <- liftIO $ DL.takeWhile (not . isSpace) <$> readFile outFile
    dt <- liftIO $ getCurrentTime
    update' acid . UpdateRacer $ (racer & selectedBuild .~ Just bName)
    update' acid . AddRacerBuild $ RacerBuild
        { _buildName = bName
        , _buildRev = bRev
        , _buildDate = dt
        , _buildRacerId = rid
        }
    return bRev

getScriptLogs
  :: (MonadIO m, Monad m, MonadReader RepoAcid m)
  => m AS.ScriptLog
getScriptLogs = ask >>= \acid -> query' acid GetScriptLog

selectBuild
  :: (MonadIO m, Monad m, MonadThrow m, MonadReader RepoAcid m)
  => RacerId -> SHA -> m ()
selectBuild rid sha = withRacer rid $ \racer -> do
  acid <- ask
  runScript $ AS.BuildRacer rid sha
  bName <- fmap _buildName <$> (query' acid $ GetRacerBuildBySHA rid sha) 
  update' acid (UpdateRacer $ racer & selectedBuild .~ bName)
  return ()

getRacers :: (MonadIO m, MonadThrow m, MonadReader RepoAcid m) => [RacerId] -> m [Racer]
getRacers rids = do
  acid <- ask
  forM rids $ \rid -> 
    do racer <- query' acid $ GetRacerById rid
       maybe (throwM $ RacerNotFound rid) (return) racer

withRacer
  :: (Monad m, MonadIO m, MonadThrow m, MonadReader RepoAcid m)
  => RacerId -> (Racer -> m a) -> m a
withRacer rid act = do
  acid <- ask
  racer <- query' acid $ GetRacerById rid
  guardMaybe (RacerNotFound rid) racer act

guardMaybe :: (Exception e, MonadThrow m) => e -> Maybe a -> (a -> m b) -> m b
guardMaybe e x f = maybe (throwM e) f x

runScript
  :: (MonadIO m, MonadThrow m, AS.Script a, MonadReader RepoAcid m)
  => a -> m [String]
runScript = runScripts . Identity

-- | Run the given script command. Upon an ExitFailure throw a ScriptError exception
runScripts
  :: (MonadIO m, MonadThrow m, AS.Script a, Traversable t, MonadReader RepoAcid m)
  => t a -> m [String]
runScripts cmds = do
  acid <- ask
  config <- query' acid GetScriptConfig
  log <- AS.runScriptCommand config cmds
  update' acid (AddScriptLog log)
  return $ AS._stdOut <$> log

defaultRepo = RepoDBState
  { _racerDB = emptyRacerDB
  , _nextRacerId = RacerId 1
  , _scriptLog = []
  , _robotIps = ["192.168.1.2","192.168.1.3"]
  , _raceHistory = emptyRaceHistoryDB
  , _racerBuilds = emptyBuildDB
  , _scriptConfig =
    AS.ScriptConfig
    { AS._scriptBasePath = "/home/noah/src/com/ariaRacer/scripts"
    , AS._scriptCwd = "/tmp/arrun"
    }
  } 
