{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Aria where

import Aria.Types
import Aria.Repo
import Aria.RaceHistory
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.Acid.Run
import Data.Data
import Data.Aeson
       (FromJSON, ToJSON, encode, decodeStrict, toJSON, parseJSON)
import Data.Aeson.Types (Value,Parser)
import GHC.Generics
import Network.Simple.TCP
import Thread.Pool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data StartRaceCmd =
  StartRaceCmd
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data StopAllCmd =
  StopAllCmd
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data StopLaneCmd =
  StopLaneCmd Int
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data AbortLaneCmd =
  AbortLaneCmd Int
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data SetupRaceCmd =
  SetupRaceCmd [RacerId]
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data IsRacingCmd =
  IsRacingCmd
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data AriaServerConfig = AriaServerConfig
  { _ariaServerAddress :: HostName
  , _ariaServerPort :: ServiceName
  , _maxReceive :: Int
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

makeLenses ''AriaServerConfig

data BadCmdCall =
  BadCmdCall
  deriving (Eq, Ord, Show, Read, Data, Typeable)

type AriaServerApp = ReaderT AriaServerConfig

class (ToJSON a, FromJSON a) => AriaCommand a  where
  type AriaCmdResult a :: *
  type AriaCmdResult a = ()
  ariaCmd :: (MonadIO m, MonadThrow m, MonadCatch m) => a -> RepoApp m (AriaCmdResult a)

data ArCommand = 
   StartRaceCmd' StartRaceCmd
  |StopAllCmd' StopAllCmd
  |StopLaneCmd' StopLaneCmd
  |AbortLaneCmd' AbortLaneCmd
  |SetupRaceCmd' SetupRaceCmd
  |IsRacingCmd' IsRacingCmd
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data AriaThreadPoolResult = forall a. (ToJSON a) => AriaThreadPoolResult a

type AriaThreadPool = ThreadPool AriaThreadPoolResult

instance ToJSON RacerId

instance ToJSON StartRaceCmd

instance ToJSON StopAllCmd

instance ToJSON StopLaneCmd

instance ToJSON AbortLaneCmd

instance ToJSON SetupRaceCmd

instance ToJSON IsRacingCmd

instance FromJSON RacerId

instance FromJSON StartRaceCmd

instance FromJSON StopAllCmd

instance FromJSON StopLaneCmd

instance FromJSON AbortLaneCmd

instance FromJSON SetupRaceCmd

instance FromJSON IsRacingCmd

instance FromJSON ArCommand

instance Exception BadCmdCall

instance ToJSON AriaThreadPoolResult where
  toJSON (AriaThreadPoolResult a) = toJSON a

instance AriaCommand StartRaceCmd where
  ariaCmd StartRaceCmd = startRace

instance AriaCommand StopAllCmd where
  ariaCmd StopAllCmd = stopRace Abort

instance AriaCommand StopLaneCmd where
  ariaCmd (StopLaneCmd l) = stopRace . StopLane $ l

instance AriaCommand AbortLaneCmd where
  ariaCmd (AbortLaneCmd l) = stopRace . AbortLane $ l

instance AriaCommand SetupRaceCmd where
  ariaCmd (SetupRaceCmd rids) = setupRace rids

instance AriaCommand IsRacingCmd where
  type AriaCmdResult IsRacingCmd = Bool
  ariaCmd IsRacingCmd = isRacing

runAriaCommand
  :: (AriaCommand a, FromJSON (AriaCmdResult a), ToJSON a, MonadIO m, MonadMask m)
  => a -> AriaServerApp m (Maybe (AriaCmdResult a))
runAriaCommand cmd = do
  config <- ask
  d <-
    connect (config ^. ariaServerAddress) (config ^. ariaServerPort) $
    \(socket, _) -> do
      sendLazy socket $ encode cmd
      recv socket (config ^. maxReceive)
  return $ d >>= decodeStrict

serveAriaCommand :: (MonadIO m, MonadMask m, MonadThrow m) => TVar RepoAppState -> AriaServerApp m ()
serveAriaCommand stateRef = do
  config <- ask
  serve "*" (config ^. ariaServerPort) $
    \(socket, _) -> do
      d <- recv socket (config ^. maxReceive)
      cmd <- maybe (throwM BadCmdCall) (return . fromJust) (d >>= decodeStrict) 
      result <- withAriaState stateRef $ runArCommand cmd
      sendLazy socket result
  
withAriaState :: (MonadIO m, MonadCatch m, MonadThrow m) => TVar RepoAppState -> RepoApp m a -> m a
withAriaState stateRef app = do 
  state <- liftIO . atomically $ readTVar stateRef
  (r,s) <- flip runStateT state app
  liftIO . atomically $ writeTVar stateRef s 
  return r

runArCommand :: (MonadIO m, MonadCatch m, MonadThrow m) => ArCommand -> RepoApp m BSL.ByteString
runArCommand (StartRaceCmd' x) = encode <$> (ariaCmd x)
runArCommand _ = return mempty
