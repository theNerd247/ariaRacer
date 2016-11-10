{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Aria.Repo
import Aria.Repo.DB hiding (getRacers)
import Aria.RaceHistory
import Aria.Types
import Aria.Routes
import Data.SafeCopy
import Data.Data
import Data.Text (Text)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.Acid.Run
import Data.Acid.Advanced
import Data.Serialize.Put
import Data.Time (getCurrentTime)
import Data.List (intersperse)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Class (lift)
import Control.Monad.State
import Control.Lens
import Happstack.Server
import Web.Routes
import Web.Routes.Happstack
import Control.Monad.Catch
import Text.Blaze.Html (ToMarkup,toHtml)
import Pages
import Forms
import Data.Maybe (catMaybes)
import qualified Aria.Scripts as AS
import qualified Data.List as DL

type ARRunApp = RouteT Route (RepoApp (ServerPartT IO))

returnPage :: (ToMarkup a) => a -> ARRunApp Response
returnPage = return . toResponse . toHtml 

route :: Route -> ARRunApp Response
route r = case r of
  RcrRoute d -> racerRoutes d
  AdmRoute d -> admRoutes d
  `catch`
  \(AS.ScriptError log) -> returnPage $  scriptErrorPage log

admRoutes :: Maybe AdminRoute -> ARRunApp Response
admRoutes Nothing = do
  acid <- getAcid <$> get
  rs <- query' acid GetRacers
  let racers = DL.sortBy (\r1 r2 -> (r1 ^. racerId) `compare` (r2 ^. racerId)) rs
  nrForm <- newRacerForm (toPathInfo $ AdmRoute Nothing) newRacerHandle
  srForm <- setupRaceForm racers (toPathInfo $ AdmRoute Nothing) setupRaceHandle
  returnPage $  adminHomePage racers nrForm srForm
admRoutes (Just r) = adminRoute r

adminRoute :: AdminRoute -> ARRunApp Response
adminRoute ScriptLogs = do
  log <- lift getScriptLogs
  returnPage $  log
adminRoute (DelRacer rid) = do
  lift $ deleteRacer rid
  seeOtherURL (AdmRoute Nothing)
adminRoute RunRace = 
  _curRaceHistData <$> get >>= maybe (returnPage raceNotSetupPage) (\rdata -> do
    acid <- getAcid <$> get
    raceFlag <- lift $ isRacing
    racerNames <- (fmap $ view racerName) <$> (lift . getRacers $ rdata ^. histRaceData . rdRIds)
    returnPage $ runRacePage (rdata ^. raceLanes) raceFlag racerNames)
adminRoute StopAllCmd = do 
  lift . stopRace $ Abort
  seeOtherURL $ AdmRoute Nothing
adminRoute (AbortLaneCmd lane) = do
  lift . stopRace $ AbortLane lane
  seeOtherURL . AdmRoute $ Just RunRace
adminRoute (StopLaneCmd ln) = do
  lift . stopRace $ StopLane ln
  seeOtherURL . AdmRoute $ Just RunRace
adminRoute StartRaceCmd = do
  lift startRace 
  seeOtherURL . AdmRoute $ Just RunRace 
adminRoute (SetupRace rids) = do 
  builds <- computeBuilds $ rids
  lift . setupRace $ builds
  seeOtherURL . AdmRoute $ Just RunRace
  where
newRacerHandle :: NewRacerFormData -> ARRunApp Response
newRacerHandle rName = do
  lift $ newRacer $
    Racer
    { _racerName = rName
    , _racerId = RacerId 1
    , _racerBuilds = []
    , _selectedBuild = 0
    }
  seeOtherURL $ AdmRoute Nothing

setupRaceHandle :: SetupRaceFormData -> ARRunApp Response
setupRaceHandle rd = seeOtherURL . AdmRoute . Just $ SetupRace racers
  where
    racers = case rd of
      (Just r,Nothing) -> [r]
      (Nothing,Just r) -> [r]
      (Just r1,Just r2) -> [r1,r2]

racerRoutes :: RacerRoute -> ARRunApp Response
racerRoutes route = do
  acid <- getAcid <$> get
  r <- query' acid (GetRacerById $ route ^. racerRouteId)
  maybe (returnPage . noUserPage $ route ^. racerRouteId) (runRoute) r
  where
    runRoute racer =
      case route ^. actionRoute of
        Nothing -> userHomePage racer route
        (Just act) -> runRacerAction racer act

runRacerAction :: Racer -> ActionRoute -> ARRunApp Response
runRacerAction r (SelectBuild sha) = 
  do lift $ selectBuild (r ^. racerId) sha
     seeOtherURL . RcrRoute $ RacerRoute (r ^. racerId) Nothing

userHomePage :: Racer -> RacerRoute -> ARRunApp Response
userHomePage racer rte = do
  form <-
    uploadCodeForm
      (toPathInfo . RcrRoute $ rte)
      (uploadCodeHandle (racer ^. racerId))
  buildClocks <- getBuildClocks
  returnPage $ racerHomePage racer buildClocks form
  where
    getBuildClocks = do
      acid <- getAcid <$> get
      hist <- query' acid $ GetRaceHistByRId (racer ^. racerId)
      return . DL.sortBy (comparing snd) $ (toBuildClock . _histRaceData) <$> hist
    toBuildClock rHistData = (rHistData ^?! rdBuildNames . ix bcInd, rHistData ^?! rdTime . ix bcInd)
      where
        bcInd = fromJust $ DL.elemIndex (racer ^. racerId) (rHistData ^. rdRIds)

uploadCodeHandle :: RacerId -> UploadCodeFormData -> ARRunApp Response
uploadCodeHandle r d =
  do lift $ uploadCode r (ubuildTmpFile d) (ubuildName d)
     seeOtherURL . RcrRoute $ RacerRoute r Nothing 
  `catch`
  \(AS.ScriptError log) ->
     case ((log ^. AS.scriptFile . to (DL.isSubsequenceOf "commit")  == True) && log ^. AS.exitCode == 1) of
       True -> returnPage $  buildExistsPage r
       _ -> returnPage $ buildErrorPage r log

initRepo :: RepoDBState
initRepo = RepoDBState
  { _racerDB = emptyRacerDB
  , _nextRacerId = RacerId 1
  , _scriptLog = []
  , _raceHistory = emptyRaceHistoryDB
  , _scriptConfig =
    AS.ScriptConfig
    { AS._scriptBasePath = "/home/noah/src/com/ariaRacer/scripts"
    , AS._scriptCwd = "/tmp/arrun"
    }
  } 

instance Show RepoAppState where
  show = show . view curRaceHistData
  
runRoutes :: TVar RepoAppState -> Site Route (ServerPartT IO Response)
runRoutes stateRef = mkSitePI $ \showFun url -> do 
  state <- liftIO . atomically $ readTVar stateRef
  (r,s) <- flip runStateT state $ runRouteT route showFun url
  liftIO . atomically $ writeTVar stateRef s 
  return r

main = do
  withAcid (Just "/tmp/_state") initRepo $
    \acidState -> do
        siteState <- newTVarIO (RepoAppState acidState Nothing)
        simpleHTTP nullConf $
          do decodeBody (defaultBodyPolicy "/tmp" 10000000 10000000 10000000)
             mconcat $
               [ implSite "" "" $ runRoutes siteState
               , (mconcat $
                  [ (dir d $ serveDirectory DisableBrowsing [] d)
                  | d <- ["css", "js", "fonts"] ])
               ]
