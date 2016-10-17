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

type ARRunApp = RouteT Route (RepoApp (ServerPartT IO))

route :: Route -> ARRunApp Response
route r =
  case r of
    RcrRoute d -> racerRoutes d
    AdmRoute d -> admRoutes d

admRoutes :: AdminRoute -> ARRunApp Response
admRoutes = undefined

racerRoutes :: RacerRoute -> ARRunApp Response
racerRoutes route = do
  acid <- get
  r <- query' acid (GetRacerById $ route ^. racerRouteId)
  maybe (noUserPage $ route ^. racerRouteId) (runRoute) r
  where
    runRoute racer =
      case route ^. actionRoute of
        Nothing -> userHomePage racer route
        (Just act) -> runRacerAction act

runRacerAction :: ActionRoute -> ARRunApp Response
runRacerAction = undefined

noUserPage :: RacerId -> ARRunApp Response
noUserPage = return . toResponse . toHtml . NoUserPage

userHomePage :: Racer -> RacerRoute -> ARRunApp Response
userHomePage racer rte = do
  form <- uploadCodeForm (toPathInfo $ rte & actionRoute .~ (Just UploadCode)) uploadCode
  return . toResponse . toHtml $ (RacerHomePage racer form)

uploadCode :: UploadCodeFormData -> ARRunApp Response
uploadCode = undefined

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
  putStrLn . show . toPathInfo $ RcrRoute (RacerRoute (RacerId 2) Nothing)
  withAcid (Just "/tmp/_state") initRepo $
    (simpleHTTP nullConf . implSite "" "" . runRoutes)
