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
import Control.Applicative
import Control.Lens
import Web.Routes.PathInfo
import Data.Time (NominalDiffTime)

data Route
  = AdmRoute (Maybe ArCommand)
  | RcrRoute RacerRoute
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data RacerRoute = RacerRoute
  { _racerRouteId :: RacerId
  , _actionRoute :: (Maybe ActionRoute)
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data ActionRoute =
  SelectBuild SHA
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance PathInfo RacerId

instance PathInfo [RacerId]

instance PathInfo StartRaceCmd

instance PathInfo StopAllCmd

instance PathInfo AbortLaneCmd

instance PathInfo StopLaneCmd

instance PathInfo SetupRaceCmd

instance PathInfo IsRacingCmd

instance PathInfo ScriptLogCmd

instance PathInfo ArCommand

instance PathInfo Route where
  toPathSegments (AdmRoute r) = ("admin" : toPathSegments r)
  toPathSegments (RcrRoute r) = toPathSegments r
  fromPathSegments =
    AdmRoute <$ segment "admin" <*> fromPathSegments <|> RcrRoute <$> fromPathSegments

instance PathInfo (Maybe ArCommand) where
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

makeAdminRoute :: ArCommand -> Text
makeAdminRoute = toPathInfo . AdmRoute . Just . toArCommand

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
  , _actionRoute = act
  }
