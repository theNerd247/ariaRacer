{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Data.Racer
  ( RaceTime(..)
  , RaceTimes(..)
  , Racer(..)
  , Racers(..)
  , Rank
  , MatchData(..)
  , Tournament(..)
  , mkRaceTime
  , mkRacerID
  , racerName
  , racerID
  , matchRaceTimes
  , matchRacers
  , matchWinner
  , firstRacerData
  , secondRacerData
  , rank
  , mkRank
  ) where

import Control.Lens
import Data.Time
import Data.Fixed
import Data.Tree
import Data.Bool

type RacerID = Int

newtype RaceTime =
  RaceTime TimeOfDay
  deriving (Show, Eq, Ord)

data Racer = Racer
  { _racerName :: String
  , _racerID :: RacerID
  } deriving (Show, Eq, Ord)

type Racers = (Racer, Racer)

type RaceTimes = (RaceTime, RaceTime)

newtype Rank =
  Rank {_getRank :: Integer}
  deriving (Show, Eq, Ord)

type Tournament = Tree MatchData

data MatchData = MatchData
  { _matchRacers :: Racers
  , _matchRaceTimes :: RaceTimes
  } deriving (Show, Eq, Ord)

makeLenses ''MatchData

makeLenses ''Racer

mkRaceTime h m = RaceTime $ TimeOfDay 0 h m

mkRacerID :: Int -> RacerID
mkRacerID x
  | x < 0 = error "mkRacerID: x must be 0"
  | otherwise = x

mkRank :: Integer -> Rank
mkRank r = Rank $ bool r 0 (r < 0)

-- | Rank lens enforces that the rank r:  r > 0. If setting a rank with a value
-- <0 then the value will be truncated to 0.
rank :: Lens' Rank Integer
rank = lens (\(Rank r) -> r) (\_ r -> mkRank r)

type RaceWinner = Maybe Racer

-- | Represents the winners of a match. If both are true then the match was a tie.
type MatchWinner = (Bool, Bool)

-- | Getter for the winner of a race. Nothing means the race was a tie.
-- Otherwise the racer with the smaller time wins.
matchWinner :: Getter MatchData MatchWinner
matchWinner =
  to $
  \md ->
     case compare (md ^. matchRaceTimes . _1) (md ^. matchRaceTimes . _2) of
       EQ -> (True, True)
       LT -> (True, False)
       GT -> (False, True)

firstRacerData :: Lens' MatchData (Racer,RaceTime,Bool)
firstRacerData = lens getData setData
  where
    getData md = ((,,))
      (md ^. matchRacers . _1) 
      (md ^. matchRaceTimes ._1) 
      (md ^. matchWinner ._1)

    setData md (r,t,w) = md & 
        (matchRacers . _1 .~ r) 
      . (matchRaceTimes ._1 .~ t)

secondRacerData :: Lens' MatchData (Racer,RaceTime,Bool)
secondRacerData = lens getData setData
  where
    getData md = ((,,))
      (md ^. matchRacers . _2) 
      (md ^. matchRaceTimes ._2) 
      (md ^. matchWinner ._2)

    setData md (r,t,w) = md & 
        (matchRacers . _2 .~ r) 
      . (matchRaceTimes ._2 .~ t)
