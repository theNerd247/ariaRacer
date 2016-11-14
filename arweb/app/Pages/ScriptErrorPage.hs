{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pages.ScriptErrorPage where

import Aria.Types
import Aria.Routes
import Web.Routes.PathInfo (toPathInfo)
import HtmlTemplates
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

scriptErrorPage :: AS.ScriptLogData -> AriaWebApp H.Html
scriptErrorPage log = appTemplate "Script Error" $
  BH.jumbotron (H.string "Uh oh! A script error occured") $
  do H.h3 . H.string $ "Let the prof know an error has occured."
     H.h3 . H.string $ "The script log is: "
     H.toHtml $ log
