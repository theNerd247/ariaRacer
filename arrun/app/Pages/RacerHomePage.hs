{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pages.RacerHomePage where

import Aria.Types
import Aria.Routes
import Aria.RaceHistory
import Web.Routes.PathInfo (toPathInfo)
import Control.Lens
import HtmlTemplates
import Data.Maybe (fromJust)
import Data.Data
import Data.Text
import Data.Time (UTCTime(..))
import Text.Blaze ((!), string)
import Data.Monoid ((<>))
import Control.Monad
import qualified Aria.Scripts as AS
import qualified Data.List as DL
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Bootstrap as BH

racerHomePage :: Racer -> [(Text,RaceClock)] -> H.Html -> H.Html
racerHomePage racer clks uploadForm = appTemplate (racer ^. racerName) $ do
  withBuilds $ BH.row . BH.col "xs-12" . H.h3 $
    do H.string $ "Currently Racing with Build: "
       H.text . fromJust $ racer ^? racerBuilds .
         ix (racer ^. selectedBuild . to fromInteger) .
         buildName
  BH.row . BH.col "xs-12" $ uploadForm ! A.class_ "form-inline"
  withNoBuilds $
    do BH.row . BH.col "xs-12" . H.h2 $ "You Have No Builds"
       BH.row . BH.col "xs-12" . H.h4 $
         "Upload your source code to get started."
  withBuilds $
    do BH.row $
         do BH.col "xs-3" . H.h3 $ "Build"
            BH.col "xs-3" . H.h3 $ "Commit SHA"
            BH.col "xs-3" . H.h3 $ "Fastest Time"
            BH.col "xs-3" . H.h3 $ "Build Date"
       mconcat $ ((genSelBuildHtml $ racer ^. racerId)) <$>
         (racer ^. racerBuilds)
  where
    withNoBuilds = when (DL.length (racer ^. racerBuilds) == 0)
    withBuilds = unless (DL.length (racer ^. racerBuilds) == 0)
    genSelBuildHtml :: RacerId -> RacerBuild -> H.Html
    genSelBuildHtml rid build =
      BH.row $
      do BH.col "xs-3" $ H.a !
           A.href
             (H.toValue . toPathInfo . RcrRoute . RacerRoute rid . Just . SelectBuild $
              build ^.
              buildRev) $
           H.text (build ^. buildName)
         BH.col "xs-3" $ H.string $ build ^. buildRev . to (DL.take 8)
         BH.col "xs-3" $ H.string $ maybe ("- -") (show) $ lookup (build ^. buildName) clks
         BH.col "xs-3" $ H.toHtml $ build ^. buildDate
