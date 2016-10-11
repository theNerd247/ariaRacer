{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Name
Description : Short description
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

-}
module Html.Templates.Admin
  ( adminPageTemplate
  ) where

import qualified Data.Text as T
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import qualified Text.Blaze.Bootstrap as BHtml
import Text.Blaze.Html.Operators
import Data.Time
import Data.String
import Data.Racer
import Html.Templates.Components
import Data.Monoid

adminPageTemplate =
  BHtml.row . BHtml.col "xs-12" . mconcat $ BHtml.row <$> [userSettings]

userSettings =
  BHtml.panelDefault $
  do Html.h3 $ mempty <^ "User Settings"
     mconcat [newUser]

newUser = form "newUser" [] (inputText handler "")
  where
    handler dta = 
