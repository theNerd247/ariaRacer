{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pages.AdminHomePage where

import Aria
import Aria.Routes
import HtmlTemplates
import Control.Lens
import Data.Text
import Data.Acid.Advanced
import Text.Blaze ((!), string)
import Data.Monoid ((<>))
import Web.Routes.PathInfo (toPathInfo)
import Control.Monad
import qualified Data.List as DL
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Bootstrap as BH

adminHomePage :: H.Html -> H.Html -> AriaWebApp H.Html
adminHomePage newRacerForm setupRaceForm = do
  rs <- racers
  racerData <- forM rs genRacerInfoHtml
  appTemplate "Admin" $
    do BH.row . BH.col "xs-12" $ scriptLogLink
       BH.row . BH.col "xs-12" $ BH.accordion "-one" $
         [ ( "Manage Racers"
           , do BH.row . BH.col "xs-12 " $ newRacerForm <> mconcat racerData)
         , ("Setup Race", BH.row . BH.col "xs-12" $ setupRaceForm)
         ]
  where
    scriptLogLink =
      H.a ! A.class_ "btn btn-default" !
      A.href (H.toValue . makeAdminRoute $ ScriptLogs) $
      "Script Logs"
    racers = do
      rs <- getRacerAcid >>= flip query' FetchRacers
      return $ DL.sortBy (\r1 r2 -> (r1 ^. racerId) `compare` (r2 ^. racerId)) rs
    genRacerInfoHtml racer = do
      nb <- nBuilds (racer ^. racerId)
      return . BH.row $
        do BH.col "xs-4" $ H.h3 $ H.a ! A.href (H.toValue . delRacerRt $ racer) $
             do BH.glyphicon "remove-circle"
                H.string . (" " ++) . show $ (racer ^. racerId . unRacerId)
           BH.col "xs-4" $ H.h3 $ H.a !
             A.href (H.toValue . racerHomeRoute $ (racer ^. racerId)) $
             H.text (racer ^. racerName)
           BH.col "xs-4" $ H.h3 $ H.string $ "Builds: " ++ (show nb)
    delRacerRt r = makeAdminRoute $ DelRacer (r ^. racerId)
    nBuilds :: RacerId -> AriaWebApp Int
    nBuilds rid =
      getRacerAcid >>= flip query' (GetRacerBuildsByRId rid) >>= return . DL.length
