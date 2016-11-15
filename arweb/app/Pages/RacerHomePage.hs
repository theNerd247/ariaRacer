{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pages.RacerHomePage where

import Aria
import Aria.Types
import Aria.Routes
import Aria.RaceHistory
import Web.Routes.PathInfo (toPathInfo)
import Control.Lens
import HtmlTemplates
import Data.Maybe (fromJust)
import Data.Acid
import Data.Acid.Advanced
import Data.Data
import Data.Text
import Data.Ord (comparing)
import Data.Time (UTCTime(..))
import Text.Blaze ((!), string)
import Data.Monoid ((<>))
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.Trans.Class (lift)
import qualified Aria.Scripts as AS
import qualified Data.List as DL
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Bootstrap as BH

racerHomePage :: RacerId ->  H.Html -> AriaWebApp H.Html
racerHomePage rid uploadForm = do 
  racer <- lift $ withRacer rid return 
  racerBuilds <- getRacerAcid >>= flip query' (GetRacerBuildsByRId rid)
  clks <- getBuildClocks
  appTemplate (racer ^. racerName) $
    do withBuilds racer $ BH.row . BH.col "xs-12" . H.h3 $ "Currently Racing with Build: " <> H.text (fromJust $ racer ^. selectedBuild)
       BH.row . BH.col "xs-12" $ uploadForm
       withNoBuilds racer $
         do BH.row . BH.col "xs-12" . H.h2 $ "You Have No Builds"
            BH.row . BH.col "xs-12" . H.h4 $
              "Upload your source code to get started."
       withBuilds racer $ BH.row $
         do BH.col "xs-3" . H.h3 $ "Build"
            BH.col "xs-3" . H.h3 $ "Commit SHA"
            BH.col "xs-3" . H.h3 $ "Fastest Time"
            BH.col "xs-3" . H.h3 $ "Build Date"
            mconcat $ (genSelBuildHtml (racer^.racerId) clks) <$> racerBuilds
  where
    withNoBuilds r m = maybe m (const mempty) $ r ^. selectedBuild
    withBuilds r m = maybe mempty (const m) $ r ^. selectedBuild
    genSelBuildHtml :: RacerId -> [(Text,RaceClock)]-> RacerBuild -> H.Html
    genSelBuildHtml rid clks build =
      BH.row $
      do BH.col "xs-3" $ H.a ! A.href (H.toValue . makeRacerRoute rid $ SelectBuild (build^.buildRev)) $ H.text (build^.buildName)
         BH.col "xs-3" $ H.string $ build^.buildRev . to (DL.take 8)
         BH.col "xs-3" $ H.string $ maybe ("- -") (show) $
           lookup (build ^. buildName) clks
         BH.col "xs-3" $ H.toHtml $ build ^. buildDate
    getBuildClocks = do
      hist <- getRacerAcid >>= flip query' (GetRaceHistByRId rid)
      return . DL.sortBy (comparing snd) $ toBuildClock . DL.head . DL.filter ((==rid)._rdRId) . _histRaceData <$> hist
    toBuildClock rd = (rd^.rdBuildName, rd^.rdTime)
