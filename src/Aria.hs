{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Aria
  ( module Aria.Types
  , module Aria.Commands
  , module Aria.RaceHistory
  , module Aria.Repo
  , module Aria.Repo.DB
  , AriaCommand(..)
  , ArCommand
  , AriaServerConfig(..)
  , AriaServerApp
  , defaultAriaServerConfig
  , runAriaCommand
  , serveAriaCommands
  , maxReceive
  , ariaServerAddress
  , ariaServerPort
  ) where

import Aria.Types
import Aria.Repo
import Aria.Repo.DB
import Aria.RaceHistory
import Aria.Commands
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Monad.Trans.State
import Control.Monad.Reader
import Control.Monad
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.Acid.Run
import Data.Data
import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics
import Network.Simple.TCP
import Data.SafeCopy
import Thread.Pool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Aria.Scripts as AS

data AriaServerConfig = AriaServerConfig
  { _ariaServerAddress :: HostName
  , _ariaServerPort :: ServiceName
  , _maxReceive :: Int
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

makeLenses ''AriaServerConfig

defaultAriaServerConfig = AriaServerConfig 
  { _ariaServerAddress = "127.0.0.1"
  , _ariaServerPort = "3000"
  , _maxReceive = 2048
  }

data TCPReceiveFailed =
  TCPReceiveFailed
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data DecodeError =
  DecodeError String
  deriving (Eq, Ord, Show, Read, Data, Typeable)

type AriaServerApp = ReaderT AriaServerConfig

data ArCommand
  = StartRaceCmd' StartRaceCmd
  | StopRaceCmd' StopRaceCmd
  | SetupRaceCmd' SetupRaceCmd
  | NewRacerCmd' NewRacerCmd
  | DelRacerCmd' DelRacerCmd
  | SelectBuildCmd' SelectBuildCmd
  | UploadCodeCmd' UploadCodeCmd
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data AriaException = forall a. (Exception a, ToJSON a, FromJSON a) => AriaException a

$(deriveSafeCopy 0 'base ''ArCommand)

data ToJSONResult =
  forall a. (ToJSON a) =>
            ToJSONResult a

type AriaThreadPool = ThreadPool ToJSONResult

class (ToJSON a, FromJSON a, ToJSON (AriaCmdResult a)) =>
      AriaCommand a  where
  type AriaCmdResult a :: *
  type AriaCmdResult a = ()
  ariaCmd
    :: (MonadIO m, MonadThrow m, MonadCatch m)
    => a -> RepoApp m (AriaCmdResult a)
  toArCommand :: a -> ArCommand

instance Exception TCPReceiveFailed

instance Exception DecodeError

instance ToJSON ToJSONResult where
  toJSON (ToJSONResult a) = toJSON a

instance AriaCommand StartRaceCmd where
  ariaCmd StartRaceCmd = startRace
  toArCommand = StartRaceCmd'

instance AriaCommand StopRaceCmd where
  ariaCmd (StopRaceCmd cmd) = stopRace cmd
  toArCommand = StopRaceCmd'

instance AriaCommand SetupRaceCmd where
  ariaCmd (SetupRaceCmd rids) = setupRace rids
  toArCommand = SetupRaceCmd'

instance AriaCommand NewRacerCmd where
  type AriaCmdResult NewRacerCmd = RacerId
  ariaCmd (NewRacerCmd rname) = newRacer $ Racer
      { _racerName = rname
      , _racerId = RacerId 1
      , _selectedBuild = Nothing
      }

  toArCommand = NewRacerCmd'

instance AriaCommand DelRacerCmd where
  ariaCmd (DelRacerCmd rid) = deleteRacer rid
  toArCommand = DelRacerCmd'

instance AriaCommand SelectBuildCmd where
  ariaCmd (SelectBuildCmd rid sha) = selectBuild rid sha
  toArCommand = SelectBuildCmd'

instance AriaCommand UploadCodeCmd where
  type AriaCmdResult UploadCodeCmd = SHA
  ariaCmd (UploadCodeCmd rid path buildName) = uploadCode rid path buildName
  toArCommand = UploadCodeCmd'

instance FromJSON ArCommand where
  parseJSON v = (StartRaceCmd' <$> parseJSON v)
    <|> (StopRaceCmd' <$> parseJSON v)
    <|> (SetupRaceCmd' <$> parseJSON v)
    <|> (NewRacerCmd' <$> parseJSON v)
    <|> (DelRacerCmd' <$> parseJSON v)
    <|> (SelectBuildCmd' <$> parseJSON v)
    <|> (UploadCodeCmd' <$> parseJSON v)

runAriaCommand
  :: (AriaCommand a
     ,FromJSON (AriaCmdResult a)
     ,ToJSON a
     ,MonadIO m
     ,MonadThrow m
     ,MonadReader AriaServerConfig m
     )
  => a -> m (AriaCmdResult a)
runAriaCommand cmd = do
  config <- ask
  (socket,_) <- connectSock (config ^. ariaServerAddress) (config ^. ariaServerPort)
  sendLazy socket $ encode cmd
  result <- receiveAndDecode socket (config^.maxReceive)
  closeSock socket
  return result
      
serveAriaCommands
  :: (MonadIO m, MonadThrow m, MonadCatch m, MonadReader AriaServerConfig m)
  => TVar RepoAppState -> m ()
serveAriaCommands stateRef = do
  config <- ask
  serve "*" (config ^. ariaServerPort) $
    \(socket, _) -> do
      cmd <- receiveAndDecode socket (config ^. maxReceive)
      result <- withAriaState stateRef $ runArCommand cmd
      sendLazy socket . encode $ result

receiveAndDecode :: (MonadThrow m, MonadIO m, FromJSON a) => Socket -> Int -> m a
receiveAndDecode s max = do
  a <- recv s max
  b <- maybe (throwM $ TCPReceiveFailed) return $ a
  either (throwM . DecodeError) return $ eitherDecodeStrict b

withAriaState
  :: (MonadIO m, MonadCatch m, MonadThrow m)
  => TVar RepoAppState -> RepoApp m a -> m a
withAriaState stateRef app = do
  state <- liftIO . atomically $ readTVar stateRef
  (r, s) <- flip runStateT state app
  liftIO . atomically $ writeTVar stateRef s
  return r

runArCommand
  :: (MonadIO m, MonadCatch m, MonadThrow m)
  => ArCommand -> RepoApp m ToJSONResult 
runArCommand (StartRaceCmd' x) = ToJSONResult <$> ariaCmd x
runArCommand (StopRaceCmd' x) = ToJSONResult <$> ariaCmd x
runArCommand (SetupRaceCmd' x) = ToJSONResult <$> ariaCmd x
runArCommand (NewRacerCmd' x) = ToJSONResult <$> ariaCmd x
runArCommand (DelRacerCmd' x) = ToJSONResult <$> ariaCmd x
runArCommand (SelectBuildCmd' x) = ToJSONResult <$> ariaCmd x
runArCommand (UploadCodeCmd' x) = ToJSONResult <$> ariaCmd x
