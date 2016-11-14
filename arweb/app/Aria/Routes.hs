{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Aria.Routes where

import Aria
import GHC.Generics
import Data.Data
import Data.Text (Text)
import Control.Applicative
import Control.Lens
import Web.Routes.PathInfo
import Happstack.Server.Internal.Monads (ServerPartT)
import Data.Time (NominalDiffTime)
import Web.Routes.RouteT

type AriaWebApp = RouteT Route (RepoApp (AriaServerApp (ServerPartT IO)))

data Route
  = AdmRoute (Maybe AdminRoute)
  | RcrRoute RacerRoute
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data AdminRoute
  = NewRacer
  | DelRacer RacerId
  | ScriptLogs
  | RunRace
  | StartRace
  | StopRace StopCommand
  | SetupRace [RacerId]
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data RacerRoute = RacerRoute
  { _racerRouteId :: RacerId
  , _actionRoute :: (Maybe ActionRoute)
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data ActionRoute =
  SelectBuild SHA
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance PathInfo RacerId

instance PathInfo [RacerId]

instance PathInfo AdminRoute

instance PathInfo StopCommand

instance PathInfo Route where
  toPathSegments (AdmRoute r) = ("admin" : toPathSegments r)
  toPathSegments (RcrRoute r) = toPathSegments r
  fromPathSegments =
    AdmRoute <$ segment "admin" <*> fromPathSegments <|> RcrRoute <$> fromPathSegments

instance PathInfo (Maybe AdminRoute) where
  toPathSegments Nothing = ["home"]
  toPathSegments (Just r) = toPathSegments r
  fromPathSegments = Nothing <$ segment "home" <|> Just <$> fromPathSegments

instance PathInfo RacerRoute where
  toPathSegments (RacerRoute rid ar) = toPathSegments rid ++ (toPathSegments ar)
  fromPathSegments = RacerRoute <$> fromPathSegments <*> fromPathSegments

instance PathInfo (Maybe ActionRoute) where
  toPathSegments Nothing = ["home"]
  toPathSegments (Just ar) = toPathSegments ar
  fromPathSegments = Nothing <$ segment "home" <|> Just <$> fromPathSegments

instance PathInfo ActionRoute

makeLenses ''RacerRoute

makeAdminRoute :: AdminRoute -> Text
makeAdminRoute = toPathInfo . AdmRoute . Just

adminHomeRoute :: Text
adminHomeRoute = toPathInfo . AdmRoute $ Nothing

racerHomeRoute :: RacerId -> Text
racerHomeRoute rid =
  toPathInfo . RcrRoute $
  RacerRoute
  { _racerRouteId = rid
  , _actionRoute = Nothing
  }

makeRacerRoute :: RacerId -> ActionRoute -> Text
makeRacerRoute rid act =
  toPathInfo . RcrRoute $
  RacerRoute
  { _racerRouteId = rid
  , _actionRoute = Just act
  }
