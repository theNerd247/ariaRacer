{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Aria.Types where

import Data.Text hiding (replicate, length, take)
import Data.Data
import Data.Time (UTCTime(..), NominalDiffTime)
import Data.SafeCopy
import Control.Lens
import GHC.Generics hiding (to)

type Repository = FilePath

type SHA = String

type RaceTime = Integer

type BuildName = Text

newtype RacerId = RacerId
  { _unRacerId :: Integer
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- | The main racer data types
data Racer = Racer
  { _racerName :: Text -- ^ The racer's real name
  , _racerId :: RacerId
  , _selectedBuild :: Maybe BuildName
  } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data RacerBuild = RacerBuild
  { _buildName :: BuildName
  , _buildRev :: SHA
  , _buildDate :: UTCTime
  , _buildRacerId :: RacerId
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

makeLenses ''Racer

$(deriveSafeCopy 0 'base ''Racer)

makeLenses ''RacerBuild

$(deriveSafeCopy 0 'base ''RacerBuild)

makeLenses ''RacerId

$(deriveSafeCopy 0 'base ''RacerId)
