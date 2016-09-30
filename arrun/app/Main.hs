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
import Parser
import Commands
import Data.List (intersperse)
import qualified Data.ByteString as BS
import qualified Data.Serialize as CR

data PHandle = PHandle {_pHandle :: ProcessHandle, _pName :: String}

makeLenses ''PHandle

executeFile :: FilePath -> [String] -> IO PHandle
executeFile file args = PHandle <$> runcmd <*> (pure file)
  where
    runcmd = createProcess arProccess >>= return . view _4
    arProccess = (proc file args) {cwd = Just "/home/noah/src/com/ariaRacer/scripts"}

runWorker :: ARCommand -> IO PHandle
runWorker (CreateUser n) = executeFile "create_user.sh" [n]
runWorker (RemoveUser n) = executeFile "remove_user.sh" [n]
runWorker (RenameUser n1 n2) = executeFile "rename_user.sh" [n1,n2]
runWorker (BuildUser n b) = executeFile "build_user.sh" [n,b]

type ProcessList = TVar [PHandle]

type Address = (HostPreference,ServiceName)

-- | Run a worker and save its process handle for later
worker :: ProcessList -> ARCommand -> IO ()
worker pHandles act = do 
  phandle <- runWorker act
  atomically $ modifyTVar pHandles (phandle:)

printWorkerStatus :: ProcessList -> IO ()
printWorkerStatus plist = forever $ do 
  lst <- atomically $ readTVar plist 
  sequence_ $ printProcess <$> lst
  where 
    printProcess phndl = do
      s <- getProcessExitCode $ phndl ^. pHandle
      putStrLn $ "Process " ++ (phndl ^. pName) ++ " " ++ (printStatus s)
    printStatus Nothing = "is running"
    printStatus (Just ExitSuccess) = "finished!"
    printStatus (Just (ExitFailure c)) = "failed with exit code: " ++ (show c)

-- | Action to run when recieving request. It will recieve a max of 1024 bytes.
-- The command is then parsed and an action process is spawned. 255 is sent
-- upon success. Error codes are: 
-- 100 - failed to read from socket
-- 101 - failed to parse command
respondToServer :: ProcessList -> (Socket, SockAddr) -> IO ()
respondToServer plist (socket, addr) = do 
  dt <- recv socket 1024 
  handleData dt
  where
    handleData Nothing = do 
      putStrLn $ "Connection was closed - no data recieved" 
      sendCode 100
    handleData (Just dt) = either (printError dt) launchWorker $ parseCommand dt
    printError dt s = do 
      putStrLn $ "Failed to parse data: " ++ (show dt) ++ " " ++ s
      sendCode 101
    launchWorker cmd = do 
      putStrLn $ "Launching worker: " ++ (show cmd)
      worker plist cmd 
      sendCode 255
    sendCode :: Int -> IO ()
    sendCode = send socket . CR.encode


-- | Starts up the TCP server and blocks until the stop flag is raised
server :: ProcessList -> TVar Bool -> Address -> IO ()
server plist flag (ip, socket) = do
  forkIO $ serve ip socket $ respondToServer plist
  return ()
  {-atomically $ readTVar flag >>= check-}

main = do
  tst $ CreateUser "foo" 
  tst $ RenameUser "foo" "bob"
  tst $ RemoveUser "bob"
  {-psList <- newTVarIO []-}
  {-printWorkerStatus psList-}
  {-sequence_ $ startWorker psList <$> [CreateUser (x:[]) | x <- ['a'..'g']]-}
  {-serverStop <- newTVarIO False-}
  {-forkIO $ server psList serverStop addr-}
  {-printWorkerStatus psList-}
  {-where-}
    {-addr = (Host "127.0.0.1","9000")-}
  where
    startWorker ps cmd = forkIO $ worker ps cmd
    tst cmd = runWorker cmd >>= waitForProcess . view pHandle
