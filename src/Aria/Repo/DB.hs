{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Aria.Repo.DB where

import Aria.Types
import Aria.RaceHistory
import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.SafeCopy
import Data.IxSet (IxSet, Proxy(..), ixSet, ixFun, getEQ, getOne)
import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Data
import Data.Text (Text(..))
import Data.Time (UTCTime(..))
import qualified Data.IxSet as IxSet
import qualified Aria.Scripts as AS

type RepoDB = IxSet Racer

type RaceHistoryDB = IxSet RaceHistoryData

type RacerBuildDB = IxSet RacerBuild

data RepoDBState = RepoDBState
  { _racerDB :: RepoDB
  , _nextRacerId :: RacerId
  , _scriptLog :: AS.ScriptLog
  , _scriptConfig :: AS.ScriptConfig
  , _raceHistory :: RaceHistoryDB
  , _racerBuilds :: RacerBuildDB
  } deriving (Eq, Ord, Show, Data, Typeable)

makeLenses ''RepoDBState

$(deriveSafeCopy 5 'base ''RepoDBState)

instance IxSet.Indexable Racer where
  empty =
    ixSet
      [ ixFun $ (: []) . _racerId
      , ixFun $ (: []) . _racerName
      ]

instance IxSet.Indexable RaceHistoryData where
  empty = ixSet 
    [ ixFun $ fmap _rdRId . _histRaceData
    , ixFun $ fmap _rdTime . _histRaceData
    , ixFun $ (:[]) . _histRaceDate
    , ixFun $ fmap _rdBuildName . _histRaceData
    ]

instance IxSet.Indexable RacerBuild where
  empty = ixSet 
    [ ixFun $ (:[]) . _buildRacerId
    , ixFun $ (:[]) . _buildRev
    , ixFun $ (:[]) . _buildName
    ]
      
emptyRacerDB :: RepoDB
emptyRacerDB = IxSet.empty

emptyRaceHistoryDB :: RaceHistoryDB
emptyRaceHistoryDB = IxSet.empty

emptyBuildDB :: RacerBuildDB
emptyBuildDB = IxSet.empty

insertRacer :: Racer -> Update RepoDBState RacerId
insertRacer r = do
  repo <- get
  let rid = repo ^. nextRacerId
  let updateDB = racerDB %~ IxSet.insert (r & racerId .~ rid)
  let updateRID = nextRacerId . unRacerId %~ (+ 1)
  put $ repo & updateDB & updateRID
  return $ repo ^. nextRacerId

updateRacer :: Racer -> Update RepoDBState RacerId
updateRacer r = do
  repo <- get
  put $ repo & racerDB %~ IxSet.updateIx (r ^. racerId) r
  return $ r ^. racerId

removeRacer :: RacerId -> Update RepoDBState ()
removeRacer rid = get >>= (put . (racerDB %~ IxSet.deleteIx rid))

getRacerById :: RacerId -> Query RepoDBState (Maybe Racer)
getRacerById = getRacerBy

getRacerByName :: Text -> Query RepoDBState (Maybe Racer)
getRacerByName = getRacerBy

fetchRacers :: Query RepoDBState [Racer]
fetchRacers = IxSet.toList . _racerDB <$> ask

getRacerBy
  :: (Typeable k)
  => k -> Query RepoDBState (Maybe Racer)
getRacerBy x = (_racerDB <$> ask) >>= return . getOne . getEQ x

getScriptLog :: Query RepoDBState AS.ScriptLog
getScriptLog = _scriptLog <$> ask

addScriptLog :: AS.ScriptLog -> Update RepoDBState AS.ScriptLog
addScriptLog log = do 
  modify (scriptLog %~ (log++))
  _scriptLog <$> get

getScriptConfig :: Query RepoDBState AS.ScriptConfig
getScriptConfig = _scriptConfig <$> ask

getNextRacerId :: Query RepoDBState RacerId
getNextRacerId = _nextRacerId <$> ask

addRaceHistory :: RaceHistoryData -> Update RepoDBState ()
addRaceHistory d = modify $ raceHistory %~ IxSet.insert d

getRaceHistByRId :: RacerId -> Query RepoDBState RaceHistory
getRaceHistByRId rid = (_raceHistory <$> ask) >>= return . IxSet.toList . getEQ rid

getRaceHistByName :: BuildName -> Query RepoDBState RaceHistory
getRaceHistByName bname = (_raceHistory <$> ask) >>= return . IxSet.toList . getEQ bname

addRacerBuild :: RacerBuild  -> Update RepoDBState ()
addRacerBuild rbuild = modify (racerBuilds %~ IxSet.insert rbuild)

getRacerBuildsByRId :: RacerId -> Query RepoDBState [RacerBuild]
getRacerBuildsByRId rid = (_racerBuilds <$> ask) >>= return . IxSet.toList . getEQ rid

getRacerBuildByName :: RacerId -> BuildName -> Query RepoDBState (Maybe RacerBuild)
getRacerBuildByName rid bname = (_racerBuilds <$> ask) >>= return . IxSet.getOne . IxSet.getEQ bname . IxSet.getEQ rid

getRacerBuildBySHA :: RacerId -> SHA -> Query RepoDBState (Maybe RacerBuild)
getRacerBuildBySHA rid sha = (_racerBuilds <$> ask) >>= return . IxSet.getOne . IxSet.getEQ sha . IxSet.getEQ rid

$(makeAcidic
    ''RepoDBState
    [ 'insertRacer
    , 'getRacerById
    , 'updateRacer
    , 'getRacerByName
    , 'removeRacer
    , 'getScriptLog
    , 'addScriptLog
    , 'getScriptConfig
    , 'fetchRacers
    , 'getNextRacerId
    , 'addRaceHistory
    , 'addRacerBuild
    , 'getRaceHistByRId
    , 'getRacerBuildsByRId
    , 'getRacerBuildByName
    , 'getRacerBuildBySHA
    ])
