{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Aria.Types where

import Data.Text
import Data.Data
import Data.SafeCopy
import Control.Lens

type Repository = FilePath

type CodeRevision = Int

newtype RacerId = RacerId
  { _unRacerId :: Integer
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | The main racer data types
data Racer = Racer
  { _racerName :: Text -- ^ The racer's real name
  , _racerId :: RacerId
  } deriving (Show, Read, Eq, Ord, Data, Typeable)

newtype RaceData = RaceData
  { _unRaceData :: (RacerId, RacerId)
  } deriving (Eq, Show, Ord, Read, Data, Typeable)

makeLenses ''Racer
$(deriveSafeCopy 0 'base ''Racer)

makeLenses ''RacerId
$(deriveSafeCopy 0 'base ''RacerId)

makeLenses ''RaceData
$(deriveSafeCopy 0 'base ''RaceData)

racer1 :: Lens' RaceData RacerId
racer1 = lens (\s -> s ^. unRaceData . _1) (\s r -> s & unRaceData . _1 .~ r)

racer2 :: Lens' RaceData RacerId
racer2 = lens (\s -> s ^. unRaceData . _2) (\s r -> s & unRaceData . _2 .~ r)
