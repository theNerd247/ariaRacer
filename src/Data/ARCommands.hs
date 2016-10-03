{-#LANGUAGE DeriveGeneric #-}

module Data.ARCommands where

import GHC.Generics
import Data.Serialize

type UserName = String

type GitBranch = String

data ARCommand
  = CreateUser UserName
  | RemoveUser UserName
  | BuildUser UserName
              GitBranch
  | RenameUser UserName
               UserName
  deriving (Show, Eq, Ord,Generic)

instance Serialize ARCommand
