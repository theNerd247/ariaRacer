{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pages.RunRacePage where

import Aria.Types
import Aria.Routes
import Aria.RaceHistory
import Web.Routes.PathInfo (toPathInfo)
import HtmlTemplates
import Control.Lens
import Data.Maybe (fromJust)
import Data.Data
import Data.Text hiding (length)
import Data.Time (UTCTime(..))
import Text.Blaze ((!), string)
import Data.Monoid ((<>))
import Control.Monad
import qualified Aria.Scripts as AS
import qualified Data.List as DL
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Bootstrap as BH

runRacePage :: [Int] -> Bool -> [Text] -> H.Html
runRacePage lanes raceStarted rNames =
  appTemplate "Run Race" $
  do BH.row . BH.col "xs-12" $ H.h1 "Run Race!"
     BH.row $
       do BH.col "xs-6" $ H.h2 "Lane 1" <> (H.text . maybe ("No Racer") id $ rNames ^? ix 0)
          BH.col "xs-6" $ H.h2 "Lane 2" <> (H.text . maybe ("No Racer") id $ rNames ^? ix 1)
     BH.row $ 
       do BH.col "xs-4" mempty
          unless raceStarted $ BH.col "xs-4" startButton
          when raceStarted $ BH.col "xs-4" $ stopButton "Abort Race" StopAllCmd
          BH.col "xs-4" mempty
     when raceStarted laneStops
  where
    startButton =
      H.h1 $
      H.a ! A.href (H.toValue . toPathInfo . AdmRoute $ Just  StartRaceCmd) !
      A.class_ "btn btn-lg btn-success" $
      H.string "Start"
    stopButton txt lnk =
      H.h1 $
      H.a ! A.href (H.toValue . toPathInfo . AdmRoute . Just $ lnk) !
      A.class_ "btn btn-lg btn-danger" $
      H.string txt
    laneStops =
      mconcat $
      [ BH.row $
          do BH.col "xs-6" $ stopButton ("Lane " ++ show i ++ " Finished") (StopLaneCmd i) 
             BH.col "xs-6" $ stopButton ("Abort Lane " ++ show i) (AbortLaneCmd i)
      | i <- [1 .. length lanes] ]

raceNotSetupPage :: H.Html
raceNotSetupPage = appTemplate "Race Not Setup" $
  BH.jumbotron "Race Not Setup!" $ 
    do H.string "No race has been setup yet!"
       H.a ! A.href (H.toValue . toPathInfo $ AdmRoute Nothing) $ "Setup Race"

