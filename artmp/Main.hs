{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Aria
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC

mkNewRacer :: AriaServerApp IO (RacerId)
mkNewRacer = do 
  runAriaCommand $ NewRacerCmd "Foo"

main :: IO ()
main = flip runReaderT defaultAriaServerConfig $ liftIO . putStrLn . show =<< mkNewRacer
