{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pages.NoUserPage where

import Aria.Types
import Aria.Routes
import Control.Lens
import HtmlTemplates
import Data.Data
import Text.Blaze ((!), string)
import Data.Monoid ((<>))
import Control.Monad
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Bootstrap as BH

noUserPage :: RacerId -> H.Html
noUserPage rid = appTemplate "404 No Such Page" $
  BH.jumbotron
    (H.string $ "Uh Oh! Error 404")
    (H.string $
     "Racer with id: " ++ (show $ rid ^. unRacerId) ++ " doesn't exist!")
