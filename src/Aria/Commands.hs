{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Aria.Commands where

import Aria.Types
import Aria.RaceHistory
import Data.Data
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Data.SafeCopy
import Data.Text (Text)
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

data IsRacingCmd =
  IsRacingCmd
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''IsRacingCmd)

data ScriptLogCmd =
  ScriptLogCmd
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''ScriptLogCmd)

data NewRacerCmd =
  NewRacerCmd Racer
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''NewRacerCmd)

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

data GetCurRaceDataCmd =
  GetCurRaceDataCmd 
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

$(deriveSafeCopy 0 'base ''GetCurRaceDataCmd)


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

instance ToJSON IsRacingCmd

instance FromJSON IsRacingCmd

instance ToJSON ScriptLogCmd

instance FromJSON ScriptLogCmd

instance ToJSON NewRacerCmd 

instance FromJSON NewRacerCmd

instance ToJSON DelRacerCmd

instance FromJSON DelRacerCmd

instance ToJSON SelectBuildCmd

instance FromJSON SelectBuildCmd

instance ToJSON UploadCodeCmd

instance FromJSON UploadCodeCmd

instance ToJSON GetCurRaceDataCmd

instance FromJSON GetCurRaceDataCmd

instance ToJSON AS.ScriptLogData

instance FromJSON AS.ScriptLogData

instance ToJSON RaceClock

instance FromJSON RaceClock

instance ToJSON RaceData

instance FromJSON RaceData

instance ToJSON RaceHistoryData  

instance FromJSON RaceHistoryData
