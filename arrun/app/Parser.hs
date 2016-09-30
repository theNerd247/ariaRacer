{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Name
Description : Short description
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX


-}
module Parser
  ( parseCommand
  ) where

import Data.Attoparsec.ByteString
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Commands

parseCommand :: BS.ByteString -> Either String ARCommand
parseCommand = parseOnly cmdParser

cmdParser :: Parser ARCommand
cmdParser = parseCreate <|> parseRemove <|> parseRename <|> parseBuild

parseBuild =
  parseCMD 2 "build" >>= \args -> return $ BuildUser (args !! 0) (args !! 1)

parseCreate = parseCMD 1 "create" >>= \args -> return $ CreateUser (args !! 0)

parseRemove = parseCMD 1 "remove" >>= \args -> return $ RemoveUser (args !! 0)

parseRename =
  parseCMD 2 "rename" >>= \args -> return $ RenameUser (args !! 0) (args !! 1)

parseCMD :: Int -> BS.ByteString -> Parser [String]
parseCMD n cmdName = do
  string cmdName
  wds <- count n word
  return $ BS8.unpack <$> wds
  
space = word8 32

word :: Parser BS.ByteString
word = do
  many space
  cs <- many1 $ satisfy isChar
  many space
  return $ BS.pack cs
  where
    isChar x = 
      (x >= 48) && (x <= 57) 
      || (x >= 65) && (x <= 90)
      || (x >= 97) && (x <= 122)
