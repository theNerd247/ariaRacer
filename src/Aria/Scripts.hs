{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Aria.Scripts where

import Aria.Types
import Data.Data
import Data.SafeCopy
import Data.Serialize (encodeLazy)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Writer
import Control.Monad.Reader
import System.Exit (ExitCode(..))
import System.Process
import System.FilePath ((</>))
import Data.Time (UTCTime(..), getCurrentTime)
import qualified Data.ByteString.Lazy as BSL

data ScriptConfig = ScriptConfig
  { _scriptBasePath :: FilePath
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

data ScriptCommand
  = BuildRacer RacerId CodeRevision
  | CreateRacer RacerId
  | RemoveRacer RacerId
  deriving (Read, Show, Ord, Eq, Data, Typeable)

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

type ReturnCode = Int

type ScriptLog = [ScriptLogData]

type ScriptApp m a = (MonadIO m) =>
                     WriterT ScriptLog (ReaderT ScriptConfig m) a

makeLenses ''ScriptConfig

makeLenses ''ScriptLogData

$(deriveSafeCopy 0 'base ''ScriptConfig)
$(deriveSafeCopy 0 'base ''ScriptLogData)
$(deriveSafeCopy 1 'base ''ScriptCommand)

-- | A script is mapped to a type by the command that it will run and the
-- arguments to run it with
class Script a  where
  script :: a -> (FilePath, [String])

instance Script ScriptCommand where
  script (BuildRacer (RacerId i) rev) = ("build_racer.sh", [show i,show rev])
  script (CreateRacer (RacerId i)) = ("create_racer.sh", [show i])
  script (RemoveRacer (RacerId i)) = ("remove_racer.sh", [show i])

-- | Run the command and log the result
runScript :: FilePath -> [String] -> ScriptApp m ReturnCode
runScript cmd args = do
  sTime <- liftIO getCurrentTime
  cmdPath <- ((</> cmd) . _scriptBasePath) <$> ask
  (out, stdout, stderr) <- liftIO $ readProcessWithExitCode cmdPath args ""
  eTime <- liftIO $ getCurrentTime
  let rCode = toReturnCode out
  tell
    [ ScriptLogData
      { _scriptStartTime = sTime
      , _scriptEndTime = eTime
      , _stdErr = stderr
      , _stdOut = stdout
      , _exitCode = rCode
      , _scriptFile = cmdPath
      , _scriptArgs = args
      }
    ]
  return rCode

toReturnCode :: ExitCode -> ReturnCode
toReturnCode ExitSuccess = 0
toReturnCode (ExitFailure i) = i

runScriptCommand
  :: (MonadIO m, Script a)
  => ScriptConfig -> a -> m (ReturnCode, ScriptLog)
runScriptCommand config = flip runReaderT config . runWriterT . uncurry runScript . script
