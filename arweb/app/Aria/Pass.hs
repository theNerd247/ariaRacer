{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Aria.Pass where

import Happstack.ClientSession
import Control.Lens
import Datat.Data
import Data.SafeCopy (base, deriveSafeCopy)
import Control.Monad.Catch
import Control.Monad.IO.Class
import GHC.Generics
import qualified Data.Map as DM

data LoginSessionData = LoginSessionData
  { _loginRName :: Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

data BadLoginError =
  BadLoginError Text
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data FailedLoginAttempt = FailedLoginAttempt Username Password
	deriving (Eq,Ord,Show,Read,Data,Typeable, Generic)

type LoginApp = ClientSessionT LoginSessionData

type Username = String

type Password = String

type UNamePassMap = DM.Map String String

makeLenses ''LoginSessionData

$(deriveSafeCopy 0 'base ''LoginSessionData)

instance ClientSession LoginSessionData where
  emptySession =
    LoginSessionData
    { _loginRName = ""
    }

instance Exception BadLoginError

instance Exception FailedLoginAttempt 

guardLogin :: (MonadIO m, MonadThrow m) => Text -> m a -> LoginApp m a
guardLogin login act = do
  loginName <- getSession 
  case (loginName == login) of
    True -> act
    _ -> throwM $ BadLoginError loginName

login :: (MonadIO m, MonadThrow m) => (Username,Password) -> LoginApp m ()
login upass@(uname,pass) = do
  unless (checkLogin upass) $ throwM FailedLoginAttempt uname pass
