{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

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


data RaceClock
  = Finished RaceTime
  | Running UTCTime
  | Aborted
  deriving (Eq, Ord, Show, Data, Typeable)

type RaceClocks = [RaceClock]

data StopCommand
  = StopLane Int
  | Abort

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

makeLenses ''RacerBuild

$(deriveSafeCopy 0 'base ''RacerBuild)

makeLenses ''RaceHistoryData

$(deriveSafeCopy 0 'base ''RaceHistoryData)

makeRaceHistory :: (MonadIO m) => [RacerId] -> m RaceHistoryData
makeRaceHistory racers = do
  clks <- startClocks 
  now <- getCurrentTime
  return $ RaceHistoryData 
    {_histRaceData = RaceData 
      {_rdRIds = racers
      ,_rdTime = clks
      }
    _histRaceDate = now
    }

stopRaceClocks :: StopCommand -> RaceHistoryData -> RaceHistoryData
stopRaceClocks cmd hist = hist & histRaceData . rdTime %~ stopClocks cmd

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
  => Int -> m RaceClocks
startClocks nRacers = liftIO $ forM [1..nRacers] (\_ -> getCurrentTime >>= return . Running)

newRaceData :: [RacerId] -> RaceData
newRaceData [] = error "Bad call to newRaceData"
newRaceData rs =
  RaceData
  { _rdRIds = rids
  , _rdTime = replicate (length rids) 0
  }
  where
    rids = take 2 rs
