module Thread.Pool where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad

data ThreadPool = ThreadPool
  { addJob :: IO () -> IO ()
  , workerThreads :: [ThreadId]
  }

startPool :: Int -> IO ThreadPool
startPool n = do
  input <- newTQueueIO :: IO (TQueue (IO ()))
  threads <-
    forM [1 .. n] $ \_ -> forkIO . forever $ do 
      putStrLn $ "Running thread"
      i <- atomically $ readTQueue input
      i
      return ()
      
  return $
    ThreadPool
    { addJob = \job -> do 
        putStrLn "Creating thread"
        atomically $ writeTQueue input job
    , workerThreads = threads
    }
