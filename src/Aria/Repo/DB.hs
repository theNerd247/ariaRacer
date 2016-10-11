{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Aria.Repo.DB where

import Aria.Types
import qualified Aria.Scripts as AS
import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.SafeCopy
import Data.IxSet (IxSet, Proxy(..), ixSet, ixFun, getEQ, getOne)
import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Data
import Data.Text (Text(..))
import qualified Data.IxSet as IxSet

type RepoDB = IxSet Racer

data RepoDBState = RepoDBState
  { _racerDB :: RepoDB
  , _nextRacerId :: RacerId
  , _scriptLog :: AS.ScriptLog
  , _scriptConfig :: AS.ScriptConfig
  } deriving (Eq, Ord, Show, Data, Typeable)

makeLenses ''RepoDBState

$(deriveSafeCopy 1 'base ''RepoDBState)

instance IxSet.Indexable Racer where
  empty =
    ixSet
      [ ixFun $ (: []) . _racerId
      , ixFun $ (: []) . _racerName
      ]
      
emptyRacerDB :: RepoDB
emptyRacerDB = IxSet.empty

-- | Inserts a new racer if DNE. Otherwise the existing racer is updated
upsertRacer :: Racer -> Update RepoDBState RacerId
upsertRacer r = liftQuery (getRacerById (r ^. racerId)) >>= maybe (insertRacer r) updateRacer 

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

$(makeAcidic
    ''RepoDBState
    [ 'upsertRacer
    , 'getRacerById
    , 'getRacerByName
    , 'removeRacer
    , 'getScriptLog
    , 'addScriptLog
    , 'getScriptConfig
    ])
