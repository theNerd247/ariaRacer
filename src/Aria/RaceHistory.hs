{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Aria.RaceHistory where

import Aria.Types
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Data
import Data.Text hiding (length, replicate, take)
import Control.Monad.IO.Class
import Data.SafeCopy
import GHC.Generics hiding (to)
import Data.Monoid

data RaceClock
  = Finished RaceTime
  | Running UTCTime
  | Aborted
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

type RaceClocks = [RaceClock]

data StopCommand
  = StopLane Int
  | Abort
  | AbortLane Int
  deriving (Show, Read, Ord, Eq, Data, Typeable, Generic)

data RaceData = RaceData
  { _rdRId :: RacerId
  , _rdTime :: RaceClock
  , _rdBuildName :: Text
  } deriving (Eq, Show, Ord, Data, Typeable, Generic)

data RaceHistoryData = RaceHistoryData
  { _histRaceData :: [RaceData]
  , _histRaceDate :: UTCTime
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

type RaceHistory = [RaceHistoryData]

makeLenses ''RaceData

makeLenses ''RaceHistoryData

racerIds :: Getter RaceHistoryData [RacerId]
racerIds = to $ \hd -> _rdRId <$> (hd ^. histRaceData)

$(deriveSafeCopy 0 'base ''RaceHistoryData)

$(deriveSafeCopy 0 'base ''RaceClock)

$(deriveSafeCopy 2 'base ''RaceData)

raceLanes :: Getter RaceHistoryData [Int]
raceLanes = to $ \rd -> [1 .. length (rd ^. histRaceData)]

makeRaceHistory
  :: (MonadIO m)
  => [(RacerId, Text)] -> m RaceHistoryData
makeRaceHistory racers = do
  rd <- newRaceData racers
  now <- liftIO $ getCurrentTime
  return $
    RaceHistoryData
    { _histRaceData = rd
    , _histRaceDate = now
    }

stopRaceClocks
  :: (MonadIO m)
  => StopCommand -> RaceHistoryData -> m RaceHistoryData
stopRaceClocks (Abort) hist = return $ hist & histRaceData . each . rdTime .~ Aborted
stopRaceClocks (AbortLane i) hist =
  return $ hist & histRaceData . ix (i - 1) . rdTime .~ Aborted
stopRaceClocks (StopLane i) hist = do
  now <- liftIO getCurrentTime
  return $ hist & histRaceData . ix (i - 1) . rdTime %~
    setClock (Finished . floor . toRational . diffUTCTime now)

setClock :: (UTCTime -> RaceClock) -> RaceClock -> RaceClock
setClock f (Running t) = f t
setClock _ clk = clk

startClock
  :: MonadIO m
  => m RaceClock
startClock = liftIO $ getCurrentTime >>= return . Running

newRaceData
  :: (MonadIO m)
  => [(RacerId, Text)] -> m [RaceData]
newRaceData [] = error "Bad call to newRaceData"
newRaceData rs = return . fmap mkRacerData . take 2 $ rs
  where
    mkRacerData (rid, bname) =
      RaceData
      { _rdRId = rid
      , _rdTime = Aborted
      , _rdBuildName = bname
      }

allStopped :: RaceHistoryData -> Bool
allStopped hd = getAll $ hd ^. histRaceData . each . rdTime . (to isStopped)
  where
    isStopped Aborted = All True
    isStopped (Finished _) = All True
    isStopped _ = All False
