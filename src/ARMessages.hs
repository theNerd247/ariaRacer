{-|
Module      : Name
Description : Short description
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Message interface to the backend server

-}

module ARMessages
(
)
where

import Network.Simple.TCP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Serialize
import Data.ARCommands

type Address = (HostName,ServiceName)

type ServerResponse = Int

sendCommand :: Address -> ARCommand -> IO (Maybe ServerResponse)
sendCommand (addr,port) = connect addr port . sendClientMessage . encode

sendClientMessage :: BS.ByteString -> (Socket,SockAddr) -> IO (Maybe ServerResponse)
sendClientMessage cmd (socket, addr) = do
  send socket cmd
  dt <- recv socket 1024 
  return $ fromInteger . toInteger . head . BS.unpack <$> dt
