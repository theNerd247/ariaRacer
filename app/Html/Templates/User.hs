{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Name
Description : 
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

 Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod
tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At
vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren,
no sea takimata sanctus est Lorem ipsum dolor sit amet.

-}
module Html.Templates.User
  ( userNavbar
  , userPageTemplate
  ) where

import qualified Data.Text as T
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import qualified Text.Blaze.Bootstrap as BHtml
import Data.Time
import Data.String
import Data.Racer
import Html.Templates.Components

userPageTemplate = do
  BHtml.row $
    do BHtml.col "xs-2" mempty
       BHtml.col "xs-4" $ uploadButton
       BHtml.col "xs-4" $ selectRaceForm testRaceForm
       BHtml.col "xs-2" mempty
  BHtml.row $
    do BHtml.col "xs-2" mempty
       BHtml.col "xs-8" $ raceStats
       BHtml.col "xs-2" mempty

userNavbar =
  BHtml.mainNavigation "home" (hstr "Aria Racer") $
  [("stats", hstr "Current Race"), ("user_settings", BHtml.glyphicon "user")]

uploadButton =
  Html.button ! HtmlA.type_ "button" ! HtmlA.class_ "btn btn-lg btn-success" $
  do hstr "Upload Code "
     BHtml.glyphicon "upload"

selectRaceForm
  :: (Eq k, Html.ToValue k, Html.ToMarkup v)
  => [(k, v)] -> Html.Html
selectRaceForm [] = mempty
selectRaceForm raceProgMap =
  BHtml.row . Html.form $
  do BHtml.col "xs-6" ! HtmlA.class_ "center-block" $
       BHtml.formSelect
         "Select Race Program"
         "selRaceProg"
         raceProgMap
         (Just . fst . head $ raceProgMap)
     BHtml.col "xs-6" ! HtmlA.class_ "center-block" $
       BHtml.formSubmit $ hstr "Submit"

testRaceForm :: [(String, String)]
testRaceForm =
  [ ("Prog1", "program1.cpp")
  , ("Prog2", "program2.cpp")
  , ("Prog3", "program3.cpp")
  ]

raceStats = do
  currentRace (Just t) (r1, r2)
  nextRace (r1, r2)
  racerTime (Just . Rank $ 1) r1 t
  racerTime (Just . Rank $ 2) r2 t
  racerTime (Just . Rank $ 3) r1 t
  where
    t = RaceTime $ TimeOfDay 0 2 43
    r1 = Racer "Bob Marley"
    r2 = Racer "Hobo Joe"
