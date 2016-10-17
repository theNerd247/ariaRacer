module Thread.Pool where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad

type ThreadPoolAction a = IO a

data ThreadPool a = ThreadPool
  { addJob :: ThreadPoolAction a -> IO ()
  , workerThreads :: [ThreadId]
  , getResult :: IO a
  }

startPool :: Int -> IO (ThreadPool a)
startPool n = do
  input <- newTChanIO :: IO (TChan (ThreadPoolAction a))
  output <- newTChanIO :: IO (TChan a)
  threads <-
    forM [1 .. n] $ \n -> forkIO . forever $ do 
      job <- atomically $ readTChan input
      result <- job
      atomically $ writeTChan output result
      
  return $
    ThreadPool
      { addJob = \job -> atomically $ writeTChan input job
      , getResult = atomically $ readTChan output
      , workerThreads = threads
      }
