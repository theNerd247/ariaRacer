{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Aria
import Aria.Repo
import Aria.Repo.DB hiding (getRacers)
import Aria.Routes
import Data.SafeCopy
import Data.Data
import Data.Text (Text)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.Acid.Run
import Data.Acid.Advanced
import Data.Acid.Remote
import Data.Serialize.Put
import Data.Time (getCurrentTime)
import Data.List (intersperse)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.State
import Control.Lens
import Happstack.Server
import Network
import Web.Routes
import Web.Routes.Happstack
import Control.Monad.Catch
import Text.Blaze.Html (ToMarkup,toHtml)
import Pages
import Forms
import Data.Maybe (catMaybes)
import qualified Aria.Scripts as AS
import qualified Data.List as DL

returnPage :: (ToMarkup a) => a -> AriaWebApp Response
returnPage = return . toResponse . toHtml 

route :: Route -> AriaWebApp Response
route r = case r of
  RcrRoute d -> racerRoutes d
  AdmRoute d -> admRoutes d
  `catch`
  \(AS.ScriptError log) -> returnPage =<< scriptErrorPage log

admRoutes :: Maybe AdminRoute -> AriaWebApp Response
admRoutes Nothing = do
  racers <- use raceAcid >>= flip query' FetchRacers
  nrForm <- newRacerForm (adminHomeRoute) newRacerHandle
  srForm <- setupRaceForm racers (adminHomeRoute) setupRaceHandle
  returnPage =<< adminHomePage nrForm srForm
admRoutes (Just r) = adminRoute r

adminRoute :: AdminRoute -> AriaWebApp Response
adminRoute ScriptLogs = use raceAcid >>= flip query' GetScriptLog >>= returnPage
adminRoute (DelRacer rid) = do
  runAriaCommand (DelRacerCmd rid)
  seeOtherURL (AdmRoute Nothing)
adminRoute RunRace = 
  use curRaceHistData >>= maybe (returnPage =<< raceNotSetupPage) genRunRacePage
  where
    genRunRacePage rdata = do
      raceFlag <- lift $ isRacing
      racerNames <- forM (rdata ^. racerIds) (flip withRacer $ return . _racerName)
      returnPage =<< runRacePage (rdata ^. raceLanes) raceFlag racerNames
adminRoute (StopRace cmd) = do
  runAriaCommand $ StopRaceCmd cmd
  seeOtherURL . AdmRoute $ Just RunRace
adminRoute StartRace = do
  runAriaCommand StartRaceCmd
  seeOtherURL . AdmRoute $ Just RunRace 
adminRoute (SetupRace rids) = do 
  runAriaCommand $ SetupRaceCmd rids
  seeOtherURL . AdmRoute $ Just RunRace

newRacerHandle :: NewRacerFormData -> AriaWebApp Response
newRacerHandle rName = do
  rid <- runAriaCommand $ NewRacerCmd rName
  liftIO . putStrLn . show $ rid
  seeOtherURL $ AdmRoute Nothing

setupRaceHandle :: SetupRaceFormData -> AriaWebApp Response
setupRaceHandle rd = seeOtherURL . AdmRoute . Just $ SetupRace racers
  where
    racers = case rd of
      (Just r,Nothing) -> [r]
      (Nothing,Just r) -> [r]
      (Just r1,Just r2) -> [r1,r2]

racerRoutes :: RacerRoute -> AriaWebApp Response
racerRoutes route = 
  withRacer (route^.racerRouteId) runRoute
  `catch` 
    (\(RacerNotFound rid) -> returnPage =<< noUserPage rid)
  where
    runRoute racer =
      case route ^. actionRoute of
        Nothing -> userHomePage racer route
        (Just act) -> runRacerAction racer act

runRacerAction :: Racer -> ActionRoute -> AriaWebApp Response
runRacerAction r (SelectBuild sha) = 
  do lift $ selectBuild (r ^. racerId) sha
     seeOtherURL . RcrRoute $ RacerRoute (r ^. racerId) Nothing

userHomePage :: Racer -> RacerRoute -> AriaWebApp Response
userHomePage racer rte = do
  form <-
    uploadCodeForm
      (toPathInfo . RcrRoute $ rte)
      (uploadCodeHandle (racer ^. racerId))
  returnPage =<< racerHomePage (racer^.racerId) form

uploadCodeHandle :: RacerId -> UploadCodeFormData -> AriaWebApp Response
uploadCodeHandle r d =
  do lift $ uploadCode r (ubuildTmpFile d) (ubuildName d)
     seeOtherURL . RcrRoute $ RacerRoute r Nothing 
  `catch`
  \(AS.ScriptError log) ->
     case ((log ^. AS.scriptFile . to (DL.isSubsequenceOf "commit")  == True) && log ^. AS.exitCode == 1) of
       True -> returnPage =<< buildExistsPage r
       _ -> returnPage =<< buildErrorPage r log

instance Show RepoAppState where
  show = show . view curRaceHistData
  
runRoutes :: TVar RepoAppState -> AriaServerConfig -> Site Route (ServerPartT IO Response)
runRoutes stateRef ariaServerConfig = mkSitePI $ \showFun url -> do 
  state <- liftIO . atomically $ readTVar stateRef
  (r,s) <- flip runReaderT ariaServerConfig . flip runStateT state $ runRouteT route showFun url
  liftIO . atomically $ writeTVar stateRef s 
  return r

{-data AriaWebConfig = AriaWebConfig-}
	{-{ _ariaServerAddress :: HostName-}
  {-, _ariaServerPort :: PortID-}
	{-} deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)-}

main = do
  acidState <- openRemoteState skipAuthenticationPerform "127.0.0.1" (PortNumber (3001 :: PortNumber))
  siteState <- newTVarIO (RepoAppState acidState Nothing)
  simpleHTTP nullConf $
    do decodeBody (defaultBodyPolicy "/tmp" 10000000 10000000 10000000)
       mconcat $
         [ implSite "" "" $ runRoutes siteState defaultAriaServerConfig
         , (mconcat $
            [ (dir d $ serveDirectory DisableBrowsing [] d)
            | d <- ["css", "js", "fonts"] ])
         ]
