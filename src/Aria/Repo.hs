{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Aria.Repo
  ( newRacer
  , deleteRacer
  , getScriptLogs
  , uploadCode
  , selectBuild
  , setupRace
  , startRace
  , stopRace
  , curRaceHistData
  , whenRacing
  , whenNotRacing
  , getRacers
  , withRacer
  , isRacing
  , defaultRepo
  , AS.scriptBasePath
  , AS.scriptStartTime
  , AS.scriptEndTime
  , AS.scriptFile
  , AS.scriptArgs
  , AS.stdErr
  , AS.stdOut
  , AS.exitCode
  , RepoAppState(..)
  , RepoApp(..)
  , RepoDB
  , RepoDBState(..)
  , RacerNotFound(..)
  ) where

import Aria.Repo.DB hiding (getRacers)
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

data RepoAppState = RepoAppState 
  { _raceAcid :: AcidState RepoDBState
  , _curRaceHistData :: Maybe RaceHistoryData
  }

makeLenses ''RepoAppState

type RepoApp = StateT RepoAppState

data RacerNotFound =
  RacerNotFound RacerId
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Exception RacerNotFound

data InvalidBuildError = InvalidBuildError
  RacerId
  deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Exception InvalidBuildError

data NoSelectedBuildError = NoSelectedBuildError RacerId
  deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Exception NoSelectedBuildError

newRacer
  :: (Monad m, MonadCatch m, MonadIO m, MonadThrow m)
  => Racer -> RepoApp m RacerId
newRacer racer = do 
     acid <- use raceAcid
     rid <- update' acid (InsertRacer racer)
     runScript (AS.CreateRacer rid)
     return rid 
  `catchAll`
  (\_ -> undoNewUser >> (return $ RacerId 0))

undoNewUser
  :: (Monad m, MonadIO m, MonadThrow m)
  => RepoApp m ()
undoNewUser = do
  acid <- use raceAcid
  (RacerId rid) <- query' acid (GetNextRacerId)
  update' acid (RemoveRacer . RacerId $ rid - 1)

deleteRacer
  :: (MonadIO m, MonadThrow m)
  => RacerId -> RepoApp m ()
deleteRacer rid = do
  acid <- use raceAcid
  update' acid (RemoveRacer rid)
  runScript (AS.RemoveRacer rid)
  return ()

uploadCode
  :: (MonadIO m, MonadThrow m)
  => RacerId -> FilePath -> Text -> RepoApp m ()
uploadCode rid file bName =
  withRacer rid $
  \racer -> do
    acid <- use raceAcid
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

getScriptLogs
  :: (MonadIO m, Monad m)
  => RepoApp m AS.ScriptLog
getScriptLogs = use raceAcid >>= \acid -> query' acid GetScriptLog

selectBuild
  :: (MonadIO m, Monad m, MonadThrow m)
  => RacerId -> SHA -> RepoApp m ()
selectBuild rid sha = withRacer rid $ \racer -> do
  acid <- use raceAcid
  runScript $ AS.BuildRacer rid sha
  bName <- fmap _buildName <$> (query' acid $ GetRacerBuildBySHA rid sha) 
  update' acid (UpdateRacer $ racer & selectedBuild .~ bName)
  return ()

setupRace :: (MonadIO m, MonadThrow m, Monad m) => [RacerId] -> RepoApp m ()
setupRace rids = whenNotRacing $ do
  builds <- getRacers rids >>= flip forM getRacerBuild
  raceHist <- makeRaceHistory builds
  curRaceHistData .= Just raceHist
  return ()
  where
    getRacerBuild racer = guardMaybe (NoSelectedBuildError $ racer^.racerId) (racer^.selectedBuild) $ \bName -> return (racer^.racerId,bName)

startRace :: (MonadThrow m, MonadIO m) => RepoApp m ()
startRace = whenRacing () $ \hd -> do
  newHd <- hd & histRaceData . each . rdTime %%~ const startClock
  curRaceHistData .= Just newHd
  runScript . AS.StartRace $ _rdRId <$> newHd ^. histRaceData 
  return ()

stopRace :: (MonadIO m, MonadThrow m, Monad m) => StopCommand -> RepoApp m ()
stopRace cmd = whenRacing () $ \raceHist -> do
  runScript . AS.StopRace $ toLaneNumbers cmd
  newHist <- stopRaceClocks cmd raceHist
  curRaceHistData .= Just newHist
  when (allStopped newHist) $ do 
    acid <- use raceAcid
    update' acid . AddRaceHistory $ newHist
    curRaceHistData .= Nothing
  where
    toLaneNumbers Abort = [1,2]
    toLaneNumbers (AbortLane i) = [toInteger i]
    toLaneNumbers (StopLane i) = [toInteger i]
    
whenRacing :: (Monad m) => a -> (RaceHistoryData -> RepoApp m a) -> RepoApp m a
whenRacing x f = use curRaceHistData >>= maybe (return x) f

whenNotRacing :: (Monad m) => RepoApp m () -> RepoApp m ()
whenNotRacing f = use curRaceHistData >>= maybe f (const $ return ())

isRacing :: (Monad m) => RepoApp m Bool
isRacing = use curRaceHistData >>= maybe (return False) (return . not . allStopped)

getRacers :: (MonadIO m, MonadThrow m) => [RacerId] -> RepoApp m [Racer]
getRacers rids = do
  acid <- use raceAcid
  forM rids $ \rid -> 
    do racer <- query' acid $ GetRacerById rid
       maybe (throwM $ RacerNotFound rid) (return) racer

withRacer
  :: (Monad m, MonadIO m, MonadThrow m)
  => RacerId -> (Racer -> RepoApp m a) -> RepoApp m a
withRacer rid act = do
  acid <- use raceAcid
  racer <- query' acid $ GetRacerById rid
  guardMaybe (RacerNotFound rid) racer act

guardMaybe :: (Exception e, MonadThrow m) => e -> Maybe a -> (a -> m b) -> m b
guardMaybe e x f = maybe (throwM e) f x

runScript
  :: (MonadIO m, MonadThrow m, AS.Script a)
  => a -> RepoApp m [String]
runScript = runScripts . Identity

-- | Run the given script command. Upon an ExitFailure throw a ScriptError exception
runScripts
  :: (MonadIO m, MonadThrow m, AS.Script a, Traversable t)
  => t a -> RepoApp m [String]
runScripts cmds = do
  acid <- use raceAcid
  config <- query' acid GetScriptConfig
  log <- AS.runScriptCommand config cmds
  update' acid (AddScriptLog log)
  return $ AS._stdOut <$> log

defaultRepo = RepoDBState
  { _racerDB = emptyRacerDB
  , _nextRacerId = RacerId 1
  , _scriptLog = []
  , _raceHistory = emptyRaceHistoryDB
  , _racerBuilds = emptyBuildDB
  , _scriptConfig =
    AS.ScriptConfig
    { AS._scriptBasePath = "/home/noah/src/com/ariaRacer/scripts"
    , AS._scriptCwd = "/tmp/arrun"
    }
  } 
