module Aria where

import Aria.Routes
import Thread.Pool
import Control.Concurrent

serverReqPool :: IO ThreadPool
serverReqPool = startPool 10

stopThreadPool :: [ThreadId] -> IO ()
stopThreadPool = sequence_ . fmap killThread
