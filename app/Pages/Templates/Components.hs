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
module Pages.Templates.Components
  ( currentRace
  , nextRace
  , raceBox
  , racerTime
  ) where

import qualified Data.Text as T
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import qualified Text.Blaze.Bootstrap as BHtml
import Data.String
import Pages.Types

currentRace :: Maybe RaceTime -> (Racer, Racer) -> Html.Html
currentRace = raceBox "Current Race"

nextRace :: (Racer, Racer) -> Html.Html
nextRace = raceBox "Next Race" Nothing

raceBox :: String -> Maybe RaceTime -> (Racer, Racer) -> Html.Html
raceBox ttle t (r1, r2) =
  BHtml.panelDefault . BHtml.row $
  do BHtml.col "xs-6" $ Html.h3 $ hstr ttle
     BHtml.col "xs-2" $ maybe mempty Html.toHtml t
     BHtml.col "xs-2" $ Html.toHtml r1
     BHtml.col "xs-2" $ Html.toHtml r2

racerTime :: Maybe Rank -> Racer -> RaceTime -> Html.Html
racerTime rnk r t =
  BHtml.panelDefault . BHtml.row $
  do BHtml.col "xs-6" $ mconcat $ [rankHtml rnk, Html.toHtml r]
     BHtml.col "xs-6" $ Html.toHtml t
  where
    rankHtml = maybe mempty (BHtml.badge . Html.toHtml)
