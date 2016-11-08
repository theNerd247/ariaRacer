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
  { getAcid :: AcidState RepoDBState
  , _curRaceHistData :: Maybe RaceHistoryData
  }

type RepoApp = StateT RepoAppState

data RacerNotFound =
  RacerNotFound RacerId
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Exception RacerNotFound

makeLenses ''RepoAppState

newRacer
  :: (Monad m, MonadCatch m, MonadIO m, MonadThrow m)
  => Racer -> RepoApp m RacerId
newRacer racer = do 
     acid <- getAcid <$> get
     rid <- update' acid (InsertRacer racer)
     runScript (AS.CreateRacer rid)
     return rid 
  `catchAll`
  (\_ -> undoNewUser >> (return $ RacerId 0))

undoNewUser
  :: (Monad m, MonadIO m, MonadThrow m)
  => RepoApp m ()
undoNewUser = do
  acid <- getAcid <$> get
  (RacerId rid) <- query' acid (GetNextRacerId)
  update' acid (RemoveRacer . RacerId $ rid - 1)

deleteRacer
  :: (MonadIO m, MonadThrow m)
  => RacerId -> RepoApp m ()
deleteRacer rid = do
  acid <- getAcid <$> get
  update' acid (RemoveRacer rid)
  runScript (AS.RemoveRacer rid)
  return ()

uploadCode
  :: (MonadIO m, MonadThrow m)
  => RacerId -> FilePath -> Text -> RepoApp m ()
uploadCode rid file bName =
  withRacer rid $
  \racer -> do
    acid <- getAcid <$> get
    bPath <- AS._scriptCwd <$> query' acid (GetScriptConfig)
    let outFile = bPath ++ "/racer_" ++ (show $ _unRacerId rid) ++ "_commit.out"
    runScripts $
      [ (AS.UploadCode rid file)
      , (AS.BuildRacer rid "")
      , (AS.CommitBuild rid bName outFile)
      ]
    bRev <- liftIO $ DL.takeWhile (not . isSpace) <$> readFile outFile
    dt <- liftIO $ getCurrentTime
    let newBuild =
          RacerBuild
          { _buildName = bName
          , _buildRev = bRev
          , _buildDate = dt
          }
    update' acid . UpdateRacer $
      (racer & selectedBuild .~ 0 & racerBuilds %~ (newBuild :))
    return ()

withRacer
  :: (Monad m, MonadIO m, MonadThrow m)
  => RacerId -> (Racer -> RepoApp m a) -> RepoApp m a
withRacer rid act = do
  acid <- getAcid <$> get
  racer <- query' acid $ GetRacerById rid
  case racer of
    Nothing -> throwM $ RacerNotFound rid
    Just r -> act r

getScriptLogs
  :: (MonadIO m, Monad m)
  => RepoApp m AS.ScriptLog
getScriptLogs = getAcid <$> get >>= \acid -> query' acid GetScriptLog

selectBuild
  :: (MonadIO m, Monad m, MonadThrow m)
  => RacerId -> SHA -> RepoApp m ()
selectBuild rid sha = do
  acid <- getAcid <$> get
  withRacer rid $
    \racer -> do
      runScript $ AS.BuildRacer rid sha
      update'
        acid
        (UpdateRacer $
         racer & selectedBuild .~ setSelBuild (racer ^. racerBuilds))
      return ()
  where
    setSelBuild = maybe 0 toInteger . DL.findIndex ((== sha) . _buildRev)

setupRace :: (MonadIO m, MonadThrow m, Monad m) => [(RacerId,Text)] -> RepoApp m ()
setupRace racers = whenNotRacing $ do
    raceHist <- makeRaceHistory racers
    curRaceHistData .= Just raceHist
    return ()

startRace :: (MonadThrow m, MonadIO m) => RepoApp m ()
startRace = whenRacing () $ \hd -> do
  newClocks <-  startClocks $ hd ^. histRaceData . rdTime
  curRaceHistData .= Just (hd & histRaceData . rdTime .~ newClocks)
  runScript . AS.StartRace $ hd ^. histRaceData . rdRIds 
  return ()

stopRace :: (MonadIO m, MonadThrow m, Monad m) => StopCommand -> RepoApp m ()
stopRace cmd = whenRacing () $ \raceHist -> do
  runScript . AS.StopRace $ toLaneNumbers cmd
  liftIO . putStrLn . show $ toLaneNumbers cmd
  newHist <- stopRaceClocks cmd raceHist
  curRaceHistData .= Just newHist
  liftIO . putStrLn . show $ newHist
  liftIO . putStrLn . show $ allStopped newHist
  when (allStopped newHist) $ do 
    acid <- getAcid <$> get
    update' acid . AddRaceHistory $ newHist
    liftIO . putStrLn $ "All is stopped!"
    curRaceHistData .= Nothing
  where
    toLaneNumbers Abort = [1,2]
    toLaneNumbers (StopLane i) = [toInteger i]
    
whenRacing :: (Monad m) => a -> (RaceHistoryData -> RepoApp m a) -> RepoApp m a
whenRacing x f = _curRaceHistData <$> get >>= maybe (return x) f

whenNotRacing :: (Monad m) => RepoApp m () -> RepoApp m ()
whenNotRacing f = _curRaceHistData <$> get >>= maybe f (const $ return ())

isRacing :: (Monad m) => RepoApp m Bool
isRacing = _curRaceHistData <$> get >>= maybe (return False) (return . not . allStopped)

getRacers :: (MonadIO m, MonadThrow m) => [RacerId] -> RepoApp m [Racer]
getRacers rids = do
  acid <- getAcid <$> get
  forM rids $ \rid -> 
    do racer <- query' acid $ GetRacerById rid
       maybe (throwM $ RacerNotFound rid) (return) racer

runScript
  :: (MonadIO m, MonadThrow m, AS.Script a)
  => a -> RepoApp m [String]
runScript = runScripts . Identity

-- | Run the given script command. Upon an ExitFailure throw a ScriptError exception
runScripts
  :: (MonadIO m, MonadThrow m, AS.Script a, Traversable t)
  => t a -> RepoApp m [String]
runScripts cmds = do
  acid <- getAcid <$> get
  config <- query' acid GetScriptConfig
  log <- AS.runScriptCommand config cmds
  update' acid (AddScriptLog log)
  return $ AS._stdOut <$> log
