{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Aria.Repo
import Aria.Repo.DB
import Aria.Types
import Data.SafeCopy
import Data.Data
import Data.Acid.Run
import Data.Serialize.Put
import Control.Monad.Trans.Class (lift)
import Control.Monad.State
import Happstack.Server
import Web.Routes
import Web.Routes.Happstack
import Control.Monad.Catch
import qualified Aria.Scripts as AS

type ARRunApp = RouteT Route (RepoApp (ServerPartT IO))

data Route
  = NewRacer Racer
  | DelRacer RacerId
  | BuildRacer RacerId
               CodeRevision
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance PathInfo RacerId

instance PathInfo Racer

instance PathInfo Route

instance ToMessage Racer where
  toMessage = runPutLazy . safePut

instance ToMessage RacerId where
  toMessage = runPutLazy . safePut

instance (ToMessage a, SafeCopy a) =>
         ToMessage [a] where
  toMessage = mconcat . fmap (runPutLazy . safePut)

instance ToMessage AS.ScriptLogData where
  toMessage = runPutLazy . safePut

newRacerHndl :: Racer -> ARRunApp Response
newRacerHndl r =
  (lift $ newRacer r >>= ok . toResponse) `catch`
  (\(ScriptError log) -> ok . toResponse $ log)

removeRacerHndl = undefined

buildRacerHndl = undefined

route :: Route -> ARRunApp Response
route r =
  case r of
    (NewRacer r) -> newRacerHndl r
    (DelRacer rid) -> removeRacerHndl rid
    (BuildRacer rid rev) -> buildRacerHndl rid rev

initRepo :: RepoDBState
initRepo =
  RepoDBState
  { _racerDB = emptyRacerDB
  , _nextRacerId = RacerId 1
  , _scriptLog = []
  , _scriptConfig =
    AS.ScriptConfig
    { AS._scriptBasePath = "/home/noah/src/com/ariaRacer/scripts"
    }
  }

runRoutes :: RepoAppState -> Site Route (ServerPartT IO Response)
runRoutes initState =
  mkSitePI $
  \showFun url -> flip evalStateT initState $ runRouteT route showFun url

main = do
  withAcid (Just "/tmp/_state") initRepo $
    (simpleHTTP nullConf . implSite "" "" . runRoutes)
