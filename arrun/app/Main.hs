{-#LANGUAGE TemplateHaskell #-}

{-|
Module      : Name
Description : Short description
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Process
import Control.Lens
import Network.Simple.TCP
import Control.Concurrent.STM
import System.Exit
{-import Parser-}
import Data.ARCommands
import Data.List (intersperse,delete)
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.Serialize as CR

data PHandle = PHandle {_pHandle :: ProcessHandle, _pID :: Int}

type ProcessList = TVar [PHandle]

type Address = (HostPreference,ServiceName)

type PIDCounter = TVar Int

makeLenses ''PHandle

instance Eq PHandle where
  p1 == p2 = p1 ^. pID == p2 ^. pID

executeFile :: FilePath -> [String] -> IO ProcessHandle
executeFile file args = do 
  putStrLn $ "Starting thread: " ++ file ++ " " ++ (mconcat $ intersperse " " args)
  createProcess arProccess >>= return . view _4
  where
    arProccess = (proc file args) {cwd = Just "/home/noah/src/com/ariaRacer/scripts"}

runWorker :: ARCommand -> IO ProcessHandle
runWorker (CreateUser n) = executeFile "create_user.sh" [n]
runWorker (RemoveUser n) = executeFile "remove_user.sh" [n]
runWorker (RenameUser n1 n2) = executeFile "rename_user.sh" [n1,n2]
runWorker (BuildUser n b) = executeFile "build_user.sh" [n,b]

-- | Run a worker and save its process handle for later
worker :: ProcessList -> PIDCounter -> ARCommand -> IO ThreadId
worker pHandles pidcnt act = flip forkFinally removePHandle $ do 
  phandle <- PHandle <$> runWorker act <*> (atomically genPID)
  atomically $ modifyTVar pHandles (phandle:) 
  return phandle
  where
    genPID = modifyTVar pidcnt (+1) >> readTVar pidcnt
    removePHandle (Left _) = return ()
    removePHandle (Right ph) = atomically $ modifyTVar pHandles (delete ph)

printWorkerStatus :: ProcessList -> IO ()
printWorkerStatus plist = forever $ do 
  lst <- atomically $ readTVar plist 
  sequence_ $ printProcess <$> lst
  where 
    printProcess phndl = do
      s <- getProcessExitCode $ phndl ^. pHandle
      putStrLn $ "Process " ++ (show $ phndl ^. pID) ++ " " ++ (printStatus s)
    printStatus Nothing = "is running"
    printStatus (Just ExitSuccess) = "finished!"
    printStatus (Just (ExitFailure c)) = "failed with exit code: " ++ (show c)

-- | Action to run when recieving request. It will recieve a max of 1024 bytes.
-- The command is then parsed and an action process is spawned. 255 is sent
-- upon success. Error codes are: 
-- 100 - failed to read from socket
-- 101 - failed to parse command
respondToServer :: ProcessList -> PIDCounter -> (Socket, SockAddr) -> IO ()
respondToServer plist pidcnt (socket, addr) = do 
  dt <- recv socket 1024 
  handleData dt
  where
    handleData Nothing = do 
      putStrLn $ "Connection was closed - no data recieved" 
      sendCode 100
    handleData (Just dt) = either (printError dt) launchWorker $ decode dt
    printError dt s = do 
      putStrLn $ "Failed to parse data: " ++ (show dt) ++ " " ++ s
      sendCode 101
    launchWorker cmd = do 
      putStrLn $ "Launching worker: " ++ (show cmd)
      worker plist pidcnt cmd 
      sendCode 255
    sendCode :: Int -> IO ()
    sendCode = send socket . CR.encode

-- | Starts up the TCP server and blocks until the stop flag is raised
server :: ProcessList -> PIDCounter -> TVar Bool -> Address -> IO ()
server plist pidcnt flag (ip, socket) = do
  serve ip socket $ respondToServer plist pidcnt
  return ()
  {-atomically $ readTVar flag >>= check-}

main = do
  psList <- newTVarIO []
  pidcnt <- newTVarIO 0
  serverStop <- newTVarIO False
  server psList pidcnt serverStop addr
  where
    addr = (Host "127.0.0.1","9000")
