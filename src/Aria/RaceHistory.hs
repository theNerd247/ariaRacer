{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Aria.RaceHistory where

import Aria.Types
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.Time (UTCTime,getCurrentTime, diffUTCTime)
import Data.Data
import Control.Monad.IO.Class
import Data.SafeCopy
import GHC.Generics hiding (to)
import Data.Monoid

data RaceClock
  = Finished RaceTime
  | Running UTCTime
  | Aborted
  deriving (Eq, Ord, Show, Data, Typeable)

type RaceClocks = [RaceClock]

data StopCommand
  = StopLane Int
  | Abort
  deriving (Show, Read, Ord, Eq, Data, Typeable)

data RaceData = RaceData
  { _rdRIds :: [RacerId]
  , _rdTime :: RaceClocks
  } deriving (Eq, Show, Ord, Data, Typeable, Generic)

data RaceHistoryData = RaceHistoryData
  { _histRaceData :: RaceData
  , _histRaceDate :: UTCTime
  } deriving (Eq, Ord, Show, Data, Typeable)

type RaceHistory = [RaceHistoryData]

makeLenses ''RaceData

makeLenses ''RaceHistoryData

$(deriveSafeCopy 0 'base ''RaceHistoryData)

$(deriveSafeCopy 0 'base ''RaceClock)

$(deriveSafeCopy 1 'base ''RaceData)

raceLanes :: Getter RaceHistoryData [Int]
raceLanes = to $ \rd -> [1 .. length(rd ^. histRaceData . rdRIds)]

makeRaceHistory :: (MonadIO m) => [RacerId] -> m RaceHistoryData
makeRaceHistory racers = do
  rd <- newRaceData racers
  now <- liftIO $ getCurrentTime
  return $ RaceHistoryData 
    {_histRaceData = rd 
    ,_histRaceDate = now
    }

stopRaceClocks :: (MonadIO m) => StopCommand -> RaceHistoryData -> m RaceHistoryData
stopRaceClocks cmd hist = do 
  clks <- stopClocks cmd (hist ^. histRaceData . rdTime)
  return $ hist & histRaceData . rdTime .~ clks

-- | Stop the clocks with the given command. If a race is stopping then compute
-- the amount of time the race took
stopClocks
  :: (MonadIO m)
  => StopCommand -> RaceClocks -> m RaceClocks
stopClocks Abort rc = return $ const Aborted <$> rc
stopClocks (StopLane i) rc = do 
  now <- liftIO getCurrentTime
  return $ rc & ix i %~ setClock (Finished . floor . toRational . diffUTCTime now)

setClock :: (UTCTime -> RaceClock) -> RaceClock -> RaceClock
setClock f (Running t) = f t
setClock _ clk = clk

startClocks
  :: MonadIO m
  => RaceClocks -> m RaceClocks
startClocks clks = liftIO $ forM clks (\_ -> getCurrentTime >>= return . Running)

newRaceData :: (MonadIO m) => [RacerId] -> m RaceData
newRaceData [] = error "Bad call to newRaceData"
newRaceData rs = do
  return $ RaceData
    { _rdRIds = rids
    , _rdTime = replicate (length rids) Aborted
    }
  where
    rids = take 2 rs

allStopped :: RaceHistoryData -> Bool
allStopped hd = getAll . mconcat $ (All . isStopped) <$> hd ^. histRaceData . rdTime
  where
    isStopped Aborted = True
    isStopped (Finished _) = True
    isStopped _ = False
