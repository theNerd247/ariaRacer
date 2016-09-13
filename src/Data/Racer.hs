{-#LANGUAGE TemplateHaskell #-}

module Data.Racer where
  {-( RaceTime(..)-}
  {-, Racer(..)-}
  {-, Rank(..)-}
  {-, MatchRacerData(..)-}
  {-, TourMatchData(..)-}
  {-, mkRaceTime-}
  {-) where-}

import Control.Lens
import Data.Time
import Data.Fixed

newtype RaceTime =
  RaceTime TimeOfDay

newtype Racer = Racer
  { _racerName :: String
  }

newtype Rank =
  Rank Integer

data MatchRacerData = MatchRacerData
  { _mrRacer :: Racer
  , _mrTime :: Maybe RaceTime
  , _mrWinner :: Bool
  }

newtype TourMatchData =
  TourMatchData (MatchRacerData, MatchRacerData)

makeLenses ''MatchRacerData

makeLenses ''TourMatchData

makeLenses ''Racer

mkRaceTime h m = RaceTime $ TimeOfDay 0 h m
