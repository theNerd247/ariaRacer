{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pages.BuildExistsPage where

import Aria.Types
import Aria.Routes
import HtmlTemplates
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

buildExistsPage :: RacerId -> H.Html
buildExistsPage rid = appTemplate "Build Exists" $ do
  BH.jumbotron
    (H.string "Build Already Exists and Is Already Selected")
    (racerPageButton rid "Go back")
