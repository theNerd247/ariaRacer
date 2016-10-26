{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Aria.Repo
import Aria.Repo.DB
import Aria.Types
import Aria.Routes
import Data.SafeCopy
import Data.Data
import Data.Acid.Run
import Data.Acid.Advanced
import Data.Serialize.Put
import Data.List (intersperse)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State
import Control.Lens
import Happstack.Server
import Web.Routes
import Web.Routes.Happstack
import Control.Monad.Catch
import Text.Blaze.Html (toHtml)
import HtmlTemplates
import Forms
import qualified Aria.Scripts as AS
import qualified Data.List as DL

type ARRunApp = RouteT Route (RepoApp (ServerPartT IO))

route :: Route -> ARRunApp Response
route r =
  case r of
    RcrRoute d -> racerRoutes d
    AdmRoute d -> admRoutes d

admRoutes :: Maybe AdminRoute -> ARRunApp Response
admRoutes Nothing = do
  acid <- get
  rs <- query' acid GetRacers
  let racers = DL.sortBy (\r1 r2 -> (r1 ^. racerId) `compare` (r2 ^. racerId)) rs
  form <- newRacerForm (toPathInfo $ AdmRoute Nothing) newRacerHandle
  return . toResponse . toHtml $ AdminHomePage racers form
admRoutes (Just r) = adminRoute r

adminRoute :: AdminRoute -> ARRunApp Response
adminRoute ScriptLogs = do
  log <- lift getScriptLogs
  return . toResponse . toHtml $ log
adminRoute (DelRacer rid) = do
  lift $ deleteRacer rid
  seeOtherURL (AdmRoute Nothing)

newRacerHandle :: NewRacerFormData -> ARRunApp Response
newRacerHandle rName = do
  lift $
    newRacer $
    Racer
    { _racerName = rName
    , _racerId = RacerId 1
    , _racerBuilds = []
    , _selectedBuild = 0
    }
  seeOtherURL (AdmRoute Nothing)

racerRoutes :: RacerRoute -> ARRunApp Response
racerRoutes route = do
  acid <- get
  r <- query' acid (GetRacerById $ route ^. racerRouteId)
  maybe (noUserPage $ route ^. racerRouteId) (runRoute) r
  where
    runRoute racer =
      case route ^. actionRoute of
        Nothing -> userHomePage racer route
        (Just act) -> runRacerAction racer act

runRacerAction :: Racer -> ActionRoute -> ARRunApp Response
runRacerAction r (SelectBuild sha) = do
  lift $ selectBuild (r ^. racerId) sha
  seeOtherURL . RcrRoute $ RacerRoute (r ^. racerId) Nothing

noUserPage :: RacerId -> ARRunApp Response
noUserPage = return . toResponse . toHtml . NoUserPage

userHomePage :: Racer -> RacerRoute -> ARRunApp Response
userHomePage racer rte = do
  form <-
    uploadCodeForm (toPathInfo . RcrRoute $ rte) (uploadCodeHandle (racer ^. racerId))
  return . toResponse . toHtml $ (RacerHomePage racer form)

uploadCodeHandle :: RacerId -> UploadCodeFormData -> ARRunApp Response
uploadCodeHandle r d = do
  lift $ uploadCode r (ubuildTmpFile d) (ubuildName d)
  seeOtherURL . RcrRoute $ RacerRoute r Nothing
  `catch`
  \(ScriptError log) -> return . toResponse . toHtml $ BuildErrorPage r log

initRepo :: RepoDBState
initRepo =
  RepoDBState
  { _racerDB = emptyRacerDB
  , _nextRacerId = RacerId 1
  , _scriptLog = []
  , _scriptConfig =
    AS.ScriptConfig
    { AS._scriptBasePath = "/home/noah/src/com/ariaRacer/scripts"
    , AS._scriptCwd = "/tmp/arrun"
    }
  }

runRoutes :: RepoAppState -> Site Route (ServerPartT IO Response)
runRoutes initState =
  mkSitePI $
  \showFun url -> flip evalStateT initState $ runRouteT route showFun url

main = do
  withAcid (Just "/tmp/_state") initRepo $
    \acidState -> do
      simpleHTTP nullConf $
        do decodeBody (defaultBodyPolicy "/tmp" 10000000 10000000 10000000)
           mconcat $
             [ (implSite "" "" $ runRoutes acidState)
             , (mconcat $
                [(dir d $ serveDirectory DisableBrowsing [] d)
                | d <- ["css", "js", "fonts"] ])
             ]
