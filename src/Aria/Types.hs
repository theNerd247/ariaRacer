{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Aria.Types where

import Data.Text
import Data.Data
import Data.Time (UTCTime(..))
import Data.SafeCopy
import Control.Lens
import GHC.Generics hiding (to)

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

data RaceData = 
    SingleRacerRace RacerId
  | DoubleRacerRace RacerId RacerId
  deriving (Eq, Show, Ord, Read, Data, Typeable, Generic)

makeLenses ''Racer

$(deriveSafeCopy 0 'base ''Racer)

makeLenses ''RacerId

$(deriveSafeCopy 0 'base ''RacerId)

$(deriveSafeCopy 0 'base ''RaceData)

makeLenses ''RacerBuild

$(deriveSafeCopy 0 'base ''RacerBuild)

currentBuildSHA :: Getter Racer (Maybe SHA)
currentBuildSHA = to $ \r -> r ^? racerBuilds . ix (r ^. selectedBuild . to fromInteger) . buildRev
