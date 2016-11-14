{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pages.BuildErrorPage where

import Aria.Types
import Aria.Routes
import HtmlTemplates
import Web.Routes.PathInfo (toPathInfo)
import Control.Lens
import Data.Text
import Text.Blaze ((!), string)
import Data.Monoid ((<>))
import Control.Monad
import qualified Aria.Scripts as AS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Bootstrap as BH

buildErrorPage :: RacerId -> AS.ScriptLogData -> AriaWebApp H.Html
buildErrorPage rid log =
  appTemplate "Build Error" $
  do BH.jumbotron (H.string "Error Occured While Building Your Code") $
       do BH.row . BH.col "xs-12" . H.h3 . H.string $
            "Please fix  the following errors: "
          BH.row . BH.col "xs-12" . H.pre . H.string $ log ^. AS.stdOut
          BH.row $
            do BH.col "xs-4" mempty
               BH.col "xs-4" $ racerPageButton rid "Go back"
               BH.col "xs-4" mempty
