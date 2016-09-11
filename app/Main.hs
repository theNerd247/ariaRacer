{-# LANGUAGE OverloadedStrings #-}

module Main where

import Happstack.Lite
import HtmlTemplates
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import Text.Blaze.Html5 ((!))

defaultResponse =
  serveDirectory DisableBrowsing [] "/home/noah/src/com/ariaRacer"

serverReponse = msum [dir "home" $ (ok $ toResponse testPage), defaultResponse]

testPage = pageTemplate "Home" $ Html.div ! HtmlA.class_ "starter-template" $ helloWorld

helloWorld = Html.h1 . Html.toHtml $ ("hello World" :: String)

main :: IO ()
main = serve Nothing serverReponse
