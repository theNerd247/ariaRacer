{-# LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}

module Html.Templates.Components where

import qualified Data.Text as T
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import qualified Text.Blaze.Bootstrap as BHtml
import Control.Lens
import Data.String
import Data.Time
import Data.Racer

type Navbar = Html.Html

type BodyHtml = Html.Html

data Page = Page
  { _pageTitle :: T.Text
  , _pageNavbar :: Maybe Navbar
  , _pageBody :: BodyHtml
  }

makeLenses ''Page

instance Html.ToMarkup Page where
  toMarkup page = do
    maybe mempty id $ page ^. pageNavbar 
    BHtml.container $ page ^. pageBody

instance Html.ToMarkup RaceTime where
  toMarkup (RaceTime t) = Html.toHtml $ (show $ todMin t) ++ " : " ++ (show . secs $ t)
    where
      secs = truncate . todSec :: TimeOfDay -> Integer

instance Html.ToMarkup Racer where
  toMarkup (Racer n) = BHtml.addPopOver "User Data" (BHtml.glyphicon "user") (hstr n)
    

instance Html.ToMarkup Rank where
  toMarkup (Rank n) = hstr $ show n

hstr :: String -> Html.Html
hstr = Html.toHtml

currentRace :: Maybe RaceTime -> (Racer, Racer) -> Html.Html
currentRace = raceBox "Current Race"

nextRace :: (Racer, Racer) -> Html.Html
nextRace = raceBox "Next Race" Nothing

raceBox :: String -> Maybe RaceTime -> (Racer, Racer) -> Html.Html
raceBox ttle t (r1, r2) =
  BHtml.panelDefault . BHtml.row $
  do BHtml.col "xs-4" $ Html.span ! HtmlA.class_ "h2" $ hstr ttle
     BHtml.col "xs-2" $ Html.span ! HtmlA.class_ "h2" $
       maybe mempty Html.toHtml t
     BHtml.col "xs-3" $ showRacer r1
     BHtml.col "xs-3" $ showRacer r2
  where
    showRacer r@(Racer rn) =
      Html.span ! HtmlA.class_ "h2" $
      mconcat [Html.toHtml r, hstr " ", Html.small $ hstr rn]

racerTime :: Maybe Rank -> Racer -> RaceTime -> Html.Html
racerTime rnk r@(Racer rn) t =
  BHtml.panelDefault . BHtml.row $
  do BHtml.col "xs-3" $ Html.span ! HtmlA.class_ "h2" $
       mconcat [rankHtml rnk, hstr " ", Html.toHtml r]
     BHtml.col "xs-3" $ Html.span ! HtmlA.class_ "h2" $ Html.toHtml rn
     BHtml.col "xs-6" $ Html.span ! HtmlA.class_ "h2" $ Html.toHtml t
  where
    rankHtml = maybe mempty (BHtml.badge . Html.toHtml)

matchDataIcon :: TourMatchData -> Html.Html
matchDataIcon (TourMatchData d) = BHtml.addPopOver "Match Data" matchContent mathDataHtml
  where
    matchContent =
      Html.div ! HtmlA.class_ "circle" $ Html.span ! HtmlA.class_ "h1" $
      BHtml.glyphicon "flag"
    mathDataHtml = do
      matchRacerHtml $ fst d
      matchRacerHtml $ snd d
    matchRacerHtml :: MatchRacerData -> Html.Html
    matchRacerHtml r =
      BHtml.row $
      do hstr (r ^. mrRacer . racerName)
         hstr " "
         (if r ^. mrWinner
            then winnerHtml
            else mempty)
         hstr " "
         maybe mempty Html.toHtml $ r ^. mrTime
    winnerHtml = BHtml.glyphicon "king"
