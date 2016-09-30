module Commands where

type UserName = String

type GitBranch = String

data ARCommand
  = CreateUser UserName
  | RemoveUser UserName
  | BuildUser UserName
              GitBranch
  | RenameUser UserName
               UserName deriving (Show,Eq,Ord)
