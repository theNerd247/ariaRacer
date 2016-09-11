{-#LANGUAGE OverloadedStrings #-}

{-|
Module      : Name
Description : Short description
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

 Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod
tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At
vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren,
no sea takimata sanctus est Lorem ipsum dolor sit amet.

-}
module Text.Blaze.Bootstrap
  ( stylesheet
  , javascript
  , container
  ) where

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA

container = Html.div ! HtmlA.class_ "container"

javascript src = Html.script ! HtmlA.src src $ ""

stylesheet src = Html.link ! HtmlA.href src ! HtmlA.rel "stylesheet" ! HtmlA.type_ "text/css"
