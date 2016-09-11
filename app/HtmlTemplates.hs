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
module HtmlTemplates
  ( pageTemplate
  ) where

import Data.Text
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import qualified Text.Blaze.Bootstrap as BHtml

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
jqueryJS = "include/jquery-3.1.0/js/jquery-3.1.0.min.js"

importStyleSheets = mconcat . fmap BHtml.stylesheet

importJS = mconcat . fmap BHtml.javascript

pageTemplate :: Text -> Html.Html -> Html.Html
pageTemplate ttle bdy =
  Html.docTypeHtml $
  do Html.head $
       do bootStrapMeta
          Html.title (Html.toHtml ttle)
          importStyleSheets [bootstrapCSS]
     Html.body $
       do BHtml.container bdy
          importJS [bootstrapJS, jqueryJS]
