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
import Control.Lens
import Web.Routes.PathInfo

data Route
  = AdmRoute AdminRoute
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

instance PathInfo Route

instance PathInfo AdminRoute

instance PathInfo (Maybe ActionRoute)

instance PathInfo RacerRoute

instance PathInfo ActionRoute

makeLenses ''RacerRoute
