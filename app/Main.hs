{-# LANGUAGE OverloadedStrings #-}

module Main where

import Happstack.Lite
import HtmlTemplates
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import Text.Blaze.Html5 ((!))
import Options.Applicative

type ServerPath = String

parseServerPath =
  strArgument (help "path to serve files from" <> metavar "PATH")

defaultResponse path = serveDirectory DisableBrowsing [] path

testPage = pageTemplate "Home" $ Html.div ! HtmlA.class_ "starter-template" $ helloWorld

helloWorld = Html.h1 . Html.toHtml $ ("hello World" :: String)

main :: IO ()
main = do
  serverPath <- execParser opts
  serve Nothing $
    msum [dir "home" $ (ok $ toResponse testPage), defaultResponse serverPath]
  where
    opts = info (helper <*> parseServerPath) fullDesc
