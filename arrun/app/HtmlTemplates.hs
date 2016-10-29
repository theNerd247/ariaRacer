{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HtmlTemplates where

import Aria.Types
import Aria.Routes
import Web.Routes.PathInfo (toPathInfo)
import Control.Lens
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

data Pages
  = RacerHomePage Racer
                  H.Html
  | AdminHomePage [Racer]
                  H.Html -- create racer form
                  H.Html -- setup race form
  | NoUserPage RacerId
  | ScriptErrorPage AS.ScriptLogData
  | BuildErrorPage RacerId
                   AS.ScriptLogData
  | BuildExistsPage RacerId
  | RunRacePage RaceData Bool

instance H.ToMarkup AS.ScriptLog where
  toMarkup = H.toHtml . fmap H.toHtml

instance H.ToMarkup UTCTime where
  toMarkup = H.toHtml . show

instance H.ToMarkup AS.ScriptLogData where
  toMarkup logData =
    H.div ! A.class_ "arscript-log-data" $
    do H.div ! A.class_ "arscript-command" $
         do H.span ! A.class_ "exitcode" $ H.toHtml . show $
              (logData ^. AS.exitCode)
            " "
            H.span ! A.class_ "file" $ H.toHtml . show $
              (logData ^. AS.scriptFile)
            " "
            H.span ! A.class_ "args" $ mconcat . fmap H.toHtml .
              DL.intersperse " " $
              (logData ^. AS.scriptArgs)
       H.div ! A.class_ "arscript-rundata" $
         do H.div ! A.class_ "arscript-runtimes" $
              do H.span ! A.class_ "startTime" $ H.toHtml $
                   (logData ^. AS.scriptStartTime)
                 " - "
                 H.span ! A.class_ "endTime" $ H.toHtml $
                   (logData ^. AS.scriptEndTime)
            H.div ! A.class_ "arscript-pipes" $
              do H.div ! A.class_ "stdout" $
                   do H.span $ "stdout" <> H.br <> "---------"
                      H.pre $ H.toHtml $ (logData ^. AS.stdOut)
                 H.div ! A.class_ "stderr" $
                   do H.span $ "stderr" <> H.br <> "---------"
                      H.pre $ H.toHtml $ (logData ^. AS.stdErr)

instance H.ToMarkup Pages where
  toMarkup (RacerHomePage racer uploadForm) =
    appTemplate (racer ^. racerName) $
    do withBuilds $ BH.row . BH.col "xs-12" . H.h3 $
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
                 BH.col "xs-5" . H.h3 $ "Commit SHA"
                 BH.col "xs-4" . H.h3 $ "Build Date"
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
           BH.col "xs-5" $ H.string $ build ^. buildRev . to (DL.take 8)
           BH.col "xs-4" $ H.toHtml $ build ^. buildDate
  toMarkup (AdminHomePage racers newRacerForm setupRaceForm) =
    appTemplate "Admin" . BH.accordion "-one" $
       [("Manage Racers", 
           do BH.row . BH.col "xs-12 " $ newRacerForm
              mconcat $ genRacerInfoHtml <$> racers)
       ,("Setup Race",BH.row . BH.col "xs-12" $ setupRaceForm)
       ]
    where
      genRacerInfoHtml racer =
        BH.row $
        do BH.col "xs-4" $ H.h3 $ H.a ! A.href (H.toValue . delRacerRt $ racer) $
             do BH.glyphicon "remove-circle"
                H.string . (" " ++) . show $ (racer ^. racerId . unRacerId)
           BH.col "xs-4" $ H.h3 $ H.a ! A.href (H.toValue . toPathInfo . RcrRoute $ RacerRoute (racer^.racerId) Nothing) $ H.text (racer ^. racerName)
           BH.col "xs-4" $ H.h3 $ H.string $ "Builds: " ++
             (show . DL.length $ racer ^. racerBuilds)
      delRacerRt r = toPathInfo . AdmRoute . Just $ DelRacer (r ^. racerId)
  toMarkup (NoUserPage rid) =
    appTemplate "404 No Such User" $
    BH.jumbotron
      (H.string $ "Uh Oh! Error 404")
      (H.string $ "Racer with id: " ++ (show $ rid ^. unRacerId) ++
       " doesn't exist!")
  toMarkup (ScriptErrorPage log) =
    appTemplate "Script Error" $
    BH.jumbotron (H.string "Uh oh! A script error occured") $
    do H.h3 . H.string $ "Let the prof know an error has occured."
       H.h3 . H.string $ "The script log is: "
       H.toHtml $ log
  toMarkup (BuildErrorPage rid log) =
    appTemplate "Build Error" $
    do BH.jumbotron (H.string "Error Occured While Building Your Code") $
         do BH.row . BH.col "xs-12" . H.h3 . H.string $
              "Please fix  the following errors: "
            BH.row . BH.col "xs-12" . H.pre . H.string $ log ^. AS.stdOut
            BH.row $
              do BH.col "xs-4" mempty
                 BH.col "xs-4" $ racerPageButton rid "Go back"
                 BH.col "xs-4" mempty
  toMarkup (BuildExistsPage rid) =
    appTemplate "Build Exists" $
    do BH.jumbotron
         (H.string "Build Already Exists and Is Already Selected")
         (racerPageButton rid "Go back")
  toMarkup (RunRacePage rd raceStarted) = appTemplate "Run Race" $ 
    do centered $ BH.col "xs-4" $ H.h1 "Run Race!"
       centered $ do  
         unless raceStarted $ BH.col "xs-2" startButton
         when raceStarted $ BH.col "xs-2" $ stopButton "Stop" StopAll
       laneStops rd
    where
      startButton = H.h1 $ H.a ! A.href (H.toValue . toPathInfo . AdmRoute . Just $ StartRace rd) ! A.class_ "btn btn-lg btn-success" $ H.string "Start"
      stopButton txt lnk = H.h1 $ H.a ! A.href (H.toValue . toPathInfo . AdmRoute . Just $ lnk) ! A.class_ "btn btn-lg btn-danger" $ H.string txt
      centered cnt = BH.row $ do
         BH.col "xs-4" mempty
         cnt
         BH.col "xs-4" mempty
      laneStops (SingleRacerRace _) = mempty
      laneStops (DoubleRacerRace _ _) = centered $ do
         BH.col "xs-2" $ stopButton "Lane 1" (StopLane 1)
         BH.col "xs-2" $ stopButton "Lane 2" (StopLane 2)

racerPageButton :: RacerId -> String -> H.Html
racerPageButton rid msg =
  H.a ! A.class_ "btn btn-success btn-large" !
  A.href (H.toValue . toPathInfo . RcrRoute $ RacerRoute rid Nothing) $
  H.string msg

appTemplate :: Text -> H.Html -> H.Html
appTemplate title page =
  H.docTypeHtml $
  do H.head $
       do bootStrapMeta
          H.title $ H.text title
          importCSS [bootstrapCSS, customCSS]
     H.body $
       mconcat [BH.container page, importJS [jqueryJS, bootstrapJS, customJS]]

bootstrapCSS :: H.AttributeValue
bootstrapCSS = "/css/bootstrap.min.css"

bootstrapJS :: H.AttributeValue
bootstrapJS = "/js/bootstrap.min.js"

bootStrapMeta :: H.Html
bootStrapMeta =
  mconcat $ ((H.meta !) . mconcat) <$>
  [ [A.charset "utf-8"]
  , [A.httpEquiv "X-UA-compatible", A.content "IE=edge"]
  , [A.name "viewport", A.content "width=device-width, initial-scale=1"]
  ]

jqueryJS :: H.AttributeValue
jqueryJS = "/js/jquery-3.1.0.min.js"

customCSS :: H.AttributeValue
customCSS = "/css/custom.css"

customJS :: H.AttributeValue
customJS = "/js/custom.js"

importCSS = mconcat . fmap BH.stylesheet

importJS = mconcat . fmap BH.javascript
