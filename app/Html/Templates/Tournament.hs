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

tournamentPageTemplate = matchDataIcon

tstTourData = TourMatchData (u1,u1)
  where
    u1 = MatchRacerData r1 (Just t) True
    r1 = Racer "Bob Marley"
    t = mkRaceTime 2 59
