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

module Html.Templates.Tournament where

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import qualified Text.Blaze.Bootstrap as BHtml
import Control.Lens
import Html.Templates.Components
import Data.Racer

tournamentPageTemplate = matchDataIcon tstData
  where 
    tstData = MatchData (r1,r2) (t1,t2)
    r1 = Racer "Foo bar" 0
    r2 = Racer "Bob Marley" 1
    t1 = mkRaceTime 2 35
    t2 = mkRaceTime 5 12
