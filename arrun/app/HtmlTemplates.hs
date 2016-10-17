{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HtmlTemplates where

import Aria.Types
import Aria.Routes
import Web.Routes.PathInfo (toPathInfo)
import Control.Lens
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
                  H.Html
  | NoUserPage RacerId

instance H.ToMarkup AS.ScriptLog where
  toMarkup = H.toHtml . fmap H.toHtml

instance H.ToMarkup UTCTime where
  toMarkup = H.toHtml . show

instance H.ToMarkup AS.ScriptLogData where
  toMarkup logData =
    H.div ! A.class_ "arscript-log-data" $
    do H.div ! A.class_ "arscript-command" $
         do H.span ! A.class_ "cmd" $
              H.toHtml . show $ (logData ^. AS.scriptCmd)
            " "
            H.span ! A.class_ "exitcode" $
              H.toHtml . show $ (logData ^. AS.exitCode)
            " "
            H.span ! A.class_ "file" $
              H.toHtml . show $ (logData ^. AS.scriptFile)
            " "
            H.span ! A.class_ "args" $
              mconcat . fmap H.toHtml . DL.intersperse " " $
              (logData ^. AS.scriptArgs)
       H.div ! A.class_ "arscript-rundata" $
         do H.div ! A.class_ "arscript-runtimes" $
              do H.span ! A.class_ "startTime" $
                   H.toHtml $ (logData ^. AS.scriptStartTime)
                 " - "
                 H.span ! A.class_ "endTime" $
                   H.toHtml $ (logData ^. AS.scriptEndTime)
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
    do BH.row . BH.col "xs-12" $ uploadForm
       mconcat $ ((genSelBuildHtml $ racer ^. racerId)) <$> (racer ^. racerBuilds) 
    where
      genSelBuildHtml :: RacerId -> RacerBuild -> H.Html
      genSelBuildHtml rid build =
        BH.row $
        do BH.col "xs-4" $
             H.a ! A.href (H.toValue $ toPathInfo $ SelectBuild rid $ build ^. buildRev) $
             H.text (build ^. buildName)
           BH.col "xs-4" $ H.string $ build ^. buildRev
           BH.col "xs-4" $ H.toHtml $ build ^. buildDate

  toMarkup (AdminHomePage racers newRacerForm) =
    appTemplate "Admin" $
    do BH.row . BH.col "xs-12 " $ newRacerForm
       mconcat $ genRacerInfoHtml <$> racers
    where
      genRacerInfoHtml racer =
        BH.row $
        do BH.col "xs-4" $
             do BH.glyphicon "remove-circle"
                H.string . show $ (racer ^. racerId . unRacerId)
           BH.col "xs-4" $ H.text (racer ^. racerName)
           BH.col "xs-4" $
             H.string $ "Builds: " ++ (show . DL.length $ racer ^. racerBuilds)

  toMarkup (NoUserPage rid) = 
    appTemplate "404 No Such User" $
      BH.jumbotron 
        (H.string $ "Uh Oh! Error 404") 
        (H.string $ "Racer with id: " ++ (show $ rid ^. unRacerId) ++ " doesn't exist!")

appTemplate :: Text -> H.Html -> H.Html
appTemplate title page =
  H.docTypeHtml $
  do H.head $
       do bootStrapMeta
          H.title $ H.text title
          importCSS [bootstrapCSS, customCSS]
     H.body $ mconcat [page, importJS [jqueryJS, bootstrapJS, customJS]]

bootstrapCSS :: H.AttributeValue
bootstrapCSS = "/css/bootstrap.min.css"

bootstrapJS :: H.AttributeValue
bootstrapJS = "/js/bootstrap.min.js"

bootStrapMeta :: H.Html
bootStrapMeta =
  mconcat $
  ((H.meta !) . mconcat) <$>
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
