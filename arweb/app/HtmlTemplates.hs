{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

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

instance H.ToMarkup AS.ScriptLog where
  toMarkup = H.toHtml . fmap H.toHtml

instance H.ToMarkup UTCTime where
  toMarkup = H.toHtml . show

instance H.ToMarkup AS.ScriptLogData where
  toMarkup logData =
    H.div ! A.class_ "arscript-log-data" $
    do H.div ! A.class_ "arscript-command" $
         do H.span ! A.class_ "exitcode" $
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

racerPageButton :: RacerId -> String -> H.Html
racerPageButton rid msg =
  H.a ! A.class_ "btn btn-success btn-large" !
  A.href (H.toValue $ racerHomeRoute rid) $
  H.string msg

appTemplate :: Text -> H.Html -> AriaWebApp H.Html
appTemplate title page =
  return . H.docTypeHtml $
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
