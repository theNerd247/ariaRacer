{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Aria.RaceController where

import Aria.Types
import Aria.RaceHistory
import Aria.Repo
import Aria.Scripts
import Data.Maybe
import Data.Acid.Advanced
import Control.Monad
import Control.Lens
import GHC.Generics hiding (to)
import System.Process
import System.IO
import Control.Exception
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import System.FilePath ((</>))
import Control.Monad.Catch
import Data.Data
import Data.Monoid (Any(..),getAny)

data RacingStatus = RaceSetup RaceHistoryData 
  | RaceStarted RaceHistoryData [Maybe ProcessHandle]
  | RaceStopped 

data NoHandleException = NoHandleException
  deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Exception NoHandleException

data BadIndexError = 
  BadIndexError Int
  deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Exception BadIndexError

data NoSelectedBuildError = NoSelectedBuildError RacerId
  deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Exception NoSelectedBuildError

whenRaceStopped :: (MonadState RacingStatus m) => m () -> m ()
whenRaceStopped x = do
  hist <- get
  case hist of
    RaceStopped -> x
    _ -> return ()

whenRaceSetup :: (MonadState RacingStatus m) => m () -> m ()
whenRaceSetup x = do
  hist <- get
  case hist of
    (RaceSetup _)  -> x
    _ -> return ()

whenRacing :: (MonadState RacingStatus m) => m () -> m ()
whenRacing x = do
  hist <- get
  case hist of
    (RaceStarted _ _) -> x
    _ -> return ()

whenNotRacing :: (MonadState RacingStatus m) => m () -> m ()
whenNotRacing x = do
  hist <- get
  case hist of
    (RaceStarted _ _) -> return ()
    _ -> x

setupRace :: (MonadIO m, MonadThrow m, MonadReader RepoAcid m, MonadState RacingStatus m) => [RacerId] -> m ()
setupRace rids = whenRaceStopped $ forM rids getRacerBuild >>= makeRaceHistory >>= put . RaceSetup 
  where
    getRacerBuild rid = withRacer rid $ \racer -> guardMaybe (NoSelectedBuildError $ racer^.racerId) (racer^.selectedBuild) $ \bName -> return (racer^.racerId,bName)

startRace :: (MonadThrow m, MonadIO m, MonadReader RepoAcid m, MonadState RacingStatus m) => m ()
startRace = whenRaceSetup $ do 
  ips <- getRacerAcid >>= flip query' GetRobotIps
  (RaceSetup hist) <- get 
  (rds,threads) <- unzip <$> forM (zip (hist^.histRaceData) ips) startRacer
  put $ RaceStarted (hist & histRaceData .~ rds) threads
  where
    startRacer (raceData,ip) = do 
      ph <- getRacerAcid >>= \ips -> runRacerCode ip (raceData^.rdRId)
      clk <- startClock
      return (raceData & rdTime .~ clk,Just ph)

runRacerCode :: (MonadReader RepoAcid m, MonadIO m) => String -> RacerId -> m ProcessHandle
runRacerCode robotIp rid = do
  runPath <- getRacerAcid >>= fmap (_scriptCwd) . flip query' GetScriptConfig
  let cmdPath = runPath</>(show $ rid^.unRacerId)</>"build"</>"ariaracer"</>"ariaracer"
  logHandle <- liftIO $ openFile ("/tmp/robot_"++(show $ rid^.unRacerId)++"_log.txt") WriteMode
  (_,_,_,ph) <- liftIO . createProcess_ "Running Robot" $ (proc cmdPath ["-rh",robotIp]) {std_out = UseHandle logHandle, create_group = True}
  return ph

stopRace :: (MonadIO m, MonadThrow m, Monad m, MonadReader RepoAcid m, MonadState RacingStatus m) => StopCommand -> m ()
stopRace cmd = whenRacing $ do
  (RaceStarted hist phs) <- get
  newHist <- stopRaceClocks cmd hist
  newPhs <- foldM stopRacer phs $ take (hist^.histRaceData . (to length)) $ toLaneNumbers cmd
  let raceFlag = stillRacing newHist newPhs
  unless raceFlag $ do 
    getRacerAcid >>= flip update' (AddRaceHistory newHist)
    put RaceStopped 
  when raceFlag $ put $ RaceStarted newHist newPhs 
  where
    stopRacer :: (MonadIO m, MonadThrow m) => [Maybe ProcessHandle] -> Int -> m [Maybe ProcessHandle]
    stopRacer ph i = do
      guardMaybe (BadIndexError i) (ph ^? ix i) $ maybe (return ()) (liftIO . interruptProcessGroupOf) 
      return $ ph & ix i .~ Nothing
    toLaneNumbers Abort = [0,1]
    toLaneNumbers (AbortLane i) = [i-1]
    toLaneNumbers (StopLane i) = [i-1]
    stillRacing hist phs = (not . allStopped $ hist) && (getAny . mconcat . fmap (Any . isJust) $ phs)

isRacing :: (Monad m, MonadState RacingStatus m) => m Bool
isRacing = do 
  status <- get 
  case status of
    (RaceStarted _ _ ) -> return True
    _ -> return False
