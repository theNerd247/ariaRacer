{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Aria.Routes where

import GHC.Generics
import Data.Data
import Aria.Types
import Control.Applicative
import Control.Lens
import Web.Routes.PathInfo

data Route
  = AdmRoute (Maybe AdminRoute)
  | RcrRoute RacerRoute
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data AdminRoute
  = NewRacer
  | DelRacer RacerId
  | ScriptLogs
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data RacerRoute = RacerRoute
  { _racerRouteId :: RacerId
  , _actionRoute :: (Maybe ActionRoute)
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data ActionRoute
  = UploadCode
  | SelectBuild RacerId
                SHA
  | BuildRacer RacerId
               SHA
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance PathInfo RacerId

instance PathInfo (RacerId, RacerId)

instance PathInfo RaceData

instance PathInfo Route where
  toPathSegments (AdmRoute r) = ("admin" : toPathSegments r)
  toPathSegments (RcrRoute r) = toPathSegments r
  fromPathSegments =
    AdmRoute <$ segment "admin" <*> fromPathSegments <|> RcrRoute <$> fromPathSegments

instance PathInfo AdminRoute

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
