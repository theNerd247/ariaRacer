{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Aria.Types where

import Data.Text
import Data.Data
import Data.Time (UTCTime(..))
import Data.SafeCopy
import Control.Lens
import GHC.Generics

type Repository = FilePath

type SHA = String

newtype RacerId = RacerId
  { _unRacerId :: Integer
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- | The main racer data types
data Racer = Racer
  { _racerName :: Text -- ^ The racer's real name
  , _racerId :: RacerId
  , _racerBuilds :: [RacerBuild]
  , _selectedBuild :: Integer
  } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data RacerBuild = RacerBuild
  { _buildName :: Text
  , _buildRev :: SHA
  , _buildDate :: UTCTime
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

newtype RaceData = RaceData
  { _unRaceData :: (RacerId, RacerId)
  } deriving (Eq, Show, Ord, Read, Data, Typeable, Generic)

makeLenses ''Racer

$(deriveSafeCopy 0 'base ''Racer)

makeLenses ''RacerId

$(deriveSafeCopy 0 'base ''RacerId)

makeLenses ''RaceData

$(deriveSafeCopy 0 'base ''RaceData)

makeLenses ''RacerBuild

$(deriveSafeCopy 0 'base ''RacerBuild)

racer1 :: Lens' RaceData RacerId
racer1 = lens (\s -> s ^. unRaceData . _1) (\s r -> s & unRaceData . _1 .~ r)

racer2 :: Lens' RaceData RacerId
racer2 = lens (\s -> s ^. unRaceData . _2) (\s r -> s & unRaceData . _2 .~ r)
