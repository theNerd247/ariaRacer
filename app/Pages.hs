{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HtmlTemplates
Description : Short description
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

-}
module Pages
  ( pageTemplate
  , userPage
  ) where

import Data.Text
import Pages.Types
import Pages.Templates.User
import Text.Blaze.Html ((!))
import Control.Lens
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import qualified Text.Blaze.Bootstrap as BHtml

userPage = pageTemplate $ Page "Home" (Just userNavbar) userPageTemplate

bootstrapCSS :: Html.AttributeValue
bootstrapCSS = "css/bootstrap.min.css"

bootstrapJS :: Html.AttributeValue
bootstrapJS = "js/bootstrap.min.js"

bootStrapMeta :: Html.Html
bootStrapMeta =
  mconcat $
  ((Html.meta !) . mconcat) <$>
  [ [HtmlA.charset "utf-8"]
  , [HtmlA.httpEquiv "X-UA-compatible", HtmlA.content "IE=edge"]
  , [HtmlA.name "viewport", HtmlA.content "width=device-width, initial-scale=1"]
  ]

jqueryJS :: Html.AttributeValue
jqueryJS = "js/jquery-3.1.0.min.js"

customCSS :: Html.AttributeValue
customCSS = "css/custom.css"

customJS :: Html.AttributeValue
customJS = "js/custom.js"

importCSS = mconcat . fmap BHtml.stylesheet

importJS = mconcat . fmap BHtml.javascript

pageTemplate :: Page -> Html.Html
pageTemplate page =
  Html.docTypeHtml $
  do Html.head $
       do bootStrapMeta
          Html.title (Html.toHtml $ page ^. pageTitle)
          importCSS [bootstrapCSS,customCSS]
     Html.body $ mconcat [(Html.toHtml page), importJS [jqueryJS,bootstrapJS,customJS]]
