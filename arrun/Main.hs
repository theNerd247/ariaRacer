{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Aria
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad (forM_)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Acid.Remote
import Data.Acid.Run
import Data.Data
import Data.List.NonEmpty
import Data.Semigroup
import Control.Monad.IO.Class
import Data.Monoid (All(..))
import GHC.Generics
import Network (PortNumber, PortID(..))
import System.Directory (doesPathExist)
import qualified Data.Yaml as DY
import qualified Aria.Scripts as AS

type Aria = ExceptT String IO

data AriaServerFileConfig = AriaServerFileConfig 
  { _networkConfig :: AriaServerConfig
  , _scriptPaths :: AS.ScriptConfig
  , _robotAdresses :: [String]
  }
  deriving (Show,Read,Data,Typeable,Generic)

makeLenses ''AriaServerFileConfig

instance DY.ToJSON AS.ScriptConfig
instance DY.FromJSON AS.ScriptConfig

instance DY.ToJSON AriaServerConfig
instance DY.FromJSON AriaServerConfig

instance DY.ToJSON AriaServerFileConfig
instance DY.FromJSON AriaServerFileConfig

configFP :: FilePath
configFP = "aria.yaml"

getConfig :: Aria AriaServerFileConfig
getConfig = do
  -- read config file
  config <- ExceptT $ DY.decodeFileEither configFP >>= return . mapEither DY.prettyPrintParseException
  -- ensure all paths in config file exist
  forM_ [config^.scriptPaths^.scriptCwd, config^.scriptPaths^.scriptBasePath] checkFileExists
  return config

mapEither :: (e -> e') -> Either e a -> Either e' a
mapEither f (Left e) = Left $ f e 
mapEither _ (Right a) = Right a

checkFileExists :: FilePath -> Aria ()
checkFileExists f = do 
  x <- liftIO $ doesPathExist f 
  case x of
    False -> throwE $ "Could not find file path: " ++ f
    _ -> return ()
 
main :: IO ()
main = do 
  parsed <- runExceptT getConfig
  case parsed of
    (Left err) -> putStrLn . show $ err
    (Right config) -> do
      putStrLn "Running the aria server with the following settings:"
      putStrLn . show $ config
      -- set the acid configuration from the config file and start our ACID
      -- interface
      withAcid (Just "/tmp/_state") 
        (defaultRepo 
          & scriptConfig .~ config^.scriptPaths
          & robotIps .~ config^.robotAdresses
        )
        $ \acidState -> do
            siteState <- newTVarIO RaceStopped
            forkIO $ acidServer skipAuthenticationCheck (PortNumber (3001 :: PortNumber)) acidState
            flip runReaderT defaultAriaServerConfig $ serveAriaCommands siteState acidState
