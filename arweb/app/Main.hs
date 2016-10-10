{-# LANGUAGE OverloadedStrings #-}

module Main where

import Happstack.Lite
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import Text.Blaze.Html5 ((!))
import Options.Applicative
import Pages

type ServerPath = String

parseServerPath =
  strArgument (help "path to serve files from" <> metavar "PATH")

defaultResponse = serveDirectory DisableBrowsing []

main :: IO ()
main = do
  serverPath <- execParser opts
  serve Nothing $
    msum
      [ dir "home" $ (ok $ toResponse userPage)
      , dir "tour" $ (ok $ toResponse tournamentPage)
      , dir "admin" $ (ok $ toResponse adminPage)
      , defaultResponse serverPath
      ]
  where
    opts = info (helper <*> parseServerPath) fullDesc
