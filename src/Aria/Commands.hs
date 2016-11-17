{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Aria.Commands where

import Aria.Types
import Aria.RaceHistory
import Data.Data
import Data.Aeson.Types
import Data.Aeson
import GHC.Generics
import Data.SafeCopy
import Data.Text (Text,isPrefixOf,isSuffixOf,isInfixOf)
import qualified Aria.Scripts as AS

data StartRaceCmd =
  StartRaceCmd
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''StartRaceCmd)

data StopRaceCmd = 
  StopRaceCmd StopCommand
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''StopCommand)

$(deriveSafeCopy 0 'base ''StopRaceCmd)

data SetupRaceCmd =
  SetupRaceCmd [RacerId]
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''SetupRaceCmd)

data NewRacerCmd =
  NewRacerCmd Text
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveSafeCopy 1 'base ''NewRacerCmd)

data DelRacerCmd =
  DelRacerCmd RacerId
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''DelRacerCmd)

data SelectBuildCmd =
  SelectBuildCmd RacerId SHA
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''SelectBuildCmd)

data UploadCodeCmd =
  UploadCodeCmd RacerId
                FilePath
                Text
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''UploadCodeCmd)

data GetCurrentRaceDataCmd = 
  GetCurrentRaceDataCmd
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

$(deriveSafeCopy 0 'base ''GetCurrentRaceDataCmd)

data IsRacingCmd = IsRacingCmd
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

$(deriveSafeCopy 0 'base ''IsRacingCmd)

instance ToJSON RacerId

instance FromJSON RacerId

instance ToJSON Racer

instance FromJSON Racer

instance ToJSON StopCommand

instance FromJSON StopCommand

instance ToJSON StartRaceCmd

instance FromJSON StartRaceCmd

instance ToJSON StopRaceCmd

instance FromJSON StopRaceCmd

instance ToJSON SetupRaceCmd

instance FromJSON SetupRaceCmd

instance ToJSON NewRacerCmd 

instance FromJSON NewRacerCmd

instance ToJSON DelRacerCmd

instance FromJSON DelRacerCmd

instance ToJSON SelectBuildCmd

instance FromJSON SelectBuildCmd

instance ToJSON UploadCodeCmd

instance FromJSON UploadCodeCmd

instance ToJSON RaceClock

instance FromJSON RaceClock

instance ToJSON RaceData

instance FromJSON RaceData

instance ToJSON RaceHistoryData

instance FromJSON RaceHistoryData

gcRaceData :: Text
gcRaceData = "GetCurrentRaceDataCmd"

instance ToJSON GetCurrentRaceDataCmd where
  toJSON x = object [("_getCurrentRaceCmd",String gcRaceData)]

instance FromJSON GetCurrentRaceDataCmd where
  parseJSON (Object obj) = do 
   x <- obj .: "_getCurrentRaceCmd"
   case (isPrefixOf gcRaceData x && isSuffixOf gcRaceData x && isInfixOf x gcRaceData) of
        True -> return GetCurrentRaceDataCmd
        _ -> mempty
  parseJSON x = typeMismatch "GetCurrentRaceDataCmd" x

isRacingData :: Text
isRacingData = "IsRacingCmd"

instance ToJSON IsRacingCmd where
  toJSON x = object [("_isRacing",String isRacingData)]

instance FromJSON IsRacingCmd where
  parseJSON (Object obj) = do 
   x <- obj .: "_isRacing"
   case (isPrefixOf isRacingData x && isSuffixOf isRacingData x && isInfixOf x isRacingData) of
        True -> return IsRacingCmd
        _ -> mempty
  parseJSON x = typeMismatch "IsRacingCmd" x
