{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Aria.Scripts where

import Aria.Types
import Data.Data
import Data.SafeCopy
import Data.Serialize (encodeLazy)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Writer
import Control.Monad.Reader
import GHC.Generics
import System.Exit (ExitCode(..))
import System.Process
import System.FilePath ((</>))
import Data.Time (UTCTime(..), getCurrentTime)
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy as BSL

data ScriptConfig = ScriptConfig
  { _scriptBasePath :: FilePath -- ^ the location of the script files. 
  , _scriptCwd :: FilePath -- ^ the location to execute the script files in. This is assumed to be the location of the git repositories the user creates
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

data ScriptCommand
  = BuildRacer RacerId
               SHA
  | CreateRacer RacerId
  | RemoveRacer RacerId
  | UploadCode RacerId
               FilePath
  | CommitBuild RacerId
                Text
                FilePath
  deriving (Read, Show, Ord, Eq, Data, Typeable, Generic)

-- | Log pretty much everything that a script does
data ScriptLogData = ScriptLogData
  { _scriptStartTime :: UTCTime
  , _scriptEndTime :: UTCTime
  , _scriptFile :: FilePath
  , _scriptArgs :: [String]
  , _stdErr :: String
  , _stdOut :: String
  , _exitCode :: ReturnCode
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

data ScriptError =
  ScriptError ScriptLogData
  deriving (Read, Show, Ord, Eq, Data, Typeable)

instance Exception ScriptError

type ReturnCode = Int

type ScriptLog = [ScriptLogData]

type ScriptApp m a = (MonadIO m) =>
                     WriterT ScriptLog (ReaderT ScriptConfig m) a

makeLenses ''ScriptConfig

makeLenses ''ScriptLogData

$(deriveSafeCopy 1 'base ''ScriptConfig)

$(deriveSafeCopy 0 'base ''ScriptLogData)

$(deriveSafeCopy 1 'base ''ScriptCommand)

-- | A script is mapped to a type by the command that it will run and the
-- arguments to run it with
class Script a  where
  script :: a -> (FilePath, [String])

instance Script ScriptCommand where
  script (BuildRacer (RacerId i) rev) = ("build_racer.sh", [show i, rev])
  script (CreateRacer (RacerId i)) = ("create_racer.sh", [show i])
  script (RemoveRacer (RacerId i)) = ("remove_racer.sh", [show i])
  script (UploadCode (RacerId i) file) = ("upload_code.sh", [show i, file])
  script (CommitBuild (RacerId i) buildName commitFile) =
    ("commit_racer.sh", [show i, unpack buildName, commitFile])

-- | Run the command and log the result
runScript
  :: (Script a, MonadThrow m)
  => a -> ScriptApp m ()
runScript cmd = do
  let (cPath, args) = script cmd
  -- get the script base path and working directory from config
  cmdPath <- ((</> cPath) . _scriptBasePath) <$> ask
  runPath <- _scriptCwd <$> ask
  -- create the process command
  let scriptProcess =
        (proc cmdPath args)
        { cwd = Just runPath
        }
  -- get the start time
  sTime <- liftIO getCurrentTime
  -- start the script & block until it's finished
  -- TODO: replace with a watchdog system
  (out, stdout, stderr) <-
    liftIO $ readCreateProcessWithExitCode scriptProcess ""
  -- get the finish time
  eTime <- liftIO getCurrentTime
  -- generate the log and return
  let rCode = toReturnCode out
  let log =
        ScriptLogData
        { _scriptStartTime = sTime
        , _scriptEndTime = eTime
        , _stdErr = stderr
        , _stdOut = stdout
        , _exitCode = rCode
        , _scriptFile = cmdPath
        , _scriptArgs = args
        }
  tell [log]
  case rCode == 0 of
    True -> return ()
    _ -> throwM $ ScriptError log

toReturnCode :: ExitCode -> ReturnCode
toReturnCode ExitSuccess = 0
toReturnCode (ExitFailure i) = i

runScriptCommand
  :: (MonadIO m, MonadThrow m, Traversable t, Script a)
  => ScriptConfig -> t a -> m ScriptLog
runScriptCommand config cmds =
  flip runReaderT config . execWriterT $ forM cmds runScript
