{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pages.AdminHomePage where

import Aria.Types
import Aria.Routes
import HtmlTemplates
import Control.Lens
import Data.Text
import Text.Blaze ((!), string)
import Data.Monoid ((<>))
import Web.Routes.PathInfo (toPathInfo)
import qualified Data.List as DL
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Bootstrap as BH

adminHomePage :: [Racer] -> H.Html -> H.Html -> H.Html
adminHomePage racers newRacerForm setupRaceForm = appTemplate "Admin" $ 
  do BH.row . BH.col "xs-12" $ H.a ! A.class_ "btn btn-default" ! A.href (H.toValue . toPathInfo . AdmRoute . Just $ ScriptLogs) $ "Script Logs"
     BH.accordion "-one" $
       [ ( "Manage Racers"
         , do BH.row . BH.col "xs-12 " $ newRacerForm
              mconcat $ genRacerInfoHtml <$> racers)
       , ("Setup Race", BH.row . BH.col "xs-12" $ setupRaceForm)
       ]
  where
    genRacerInfoHtml racer =
      BH.row $
      do BH.col "xs-4" $ H.h3 $ H.a ! A.href (H.toValue . delRacerRt $ racer) $
           do BH.glyphicon "remove-circle"
              H.string . (" " ++) . show $ (racer ^. racerId . unRacerId)
         BH.col "xs-4" $ H.h3 $ H.a !
           A.href
             (H.toValue . toPathInfo . RcrRoute $
              RacerRoute (racer ^. racerId) Nothing) $
           H.text (racer ^. racerName)
         BH.col "xs-4" $ H.h3 $ H.string $ "Builds: " ++
           (show . DL.length $ racer ^. racerBuilds)
    delRacerRt r = toPathInfo . AdmRoute . Just $ DelRacer (r ^. racerId)
