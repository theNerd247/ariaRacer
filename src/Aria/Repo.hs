{-# LANGUAGE RankNTypes #-}

module Aria.Repo where

import Aria.Types
import Aria.Repo.DB
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.Acid.Advanced
import Data.FileStore
import System.FilePath ((</>))
import System.Directory (removeDirectory)

type RepoAppState = AcidState RepoDBState

type RepoApp m a = (Monad m, MonadIO m) =>
                   StateT RepoAppState m a

getRacerRepo :: RacerId -> RepoApp m FileStore
getRacerRepo = fmap gitFileStore . getRacerRepoPath

getRacerRepoPath :: RacerId -> RepoApp m FilePath
getRacerRepoPath rid = do
  acid <- get
  basePath <- _baseRepoPath <$> query' acid GetRepoConfig
  return $ basePath </> (show $ _unRacerId rid) 

newRacer :: Racer -> RepoApp m RacerId
newRacer racer = do
  acid <- get
  rid <- update' acid (UpsertRacer racer)
  repo <- getRacerRepo rid
  liftIO $ initialize repo
  return rid

deleteRacer :: RacerId -> RepoApp m ()
deleteRacer rid = do
  acid <- get
  repo <- getRacerRepoPath rid
  update' acid (RemoveRacer rid)
  liftIO $ removeDirectory repo
