{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Html.Templates.Components where

import qualified Data.Text as T
import Data.Monoid ((<>))
import Text.Blaze.Html ((!))
import Text.Blaze.Html.Operators
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import qualified Text.Blaze.Bootstrap as BHtml
import Control.Lens
import Data.String
import Data.Time
import Data.Racer
import Data.Foldable

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
  toMarkup (RaceTime t) =
    Html.toHtml $ (show $ todMin t) ++ " : " ++ (show . secs $ t)
    where
      secs = truncate . todSec :: TimeOfDay -> Integer

instance Html.ToMarkup Rank where
  toMarkup r = hstr . show $ r ^. rank

racerNameHtml :: Getter Racer Html.Html
racerNameHtml = to $ \r -> hstr $ r ^. racerName

-- | HTML icon from racer data. When clicked this will show a popuop of the data
-- for the racer.
racerIcon =
  BHtml.addPopOver "User Data" (BHtml.glyphicon "user") .
  (hstr . (view racerName))

-- | Convienience function to convert a string to HTML
hstr :: String -> Html.Html
hstr = Html.toHtml

-- | Shows the current race data in a panel
currentRace :: RaceTime -> Racers -> Html.Html
currentRace = raceBox "Current Race" . Just

-- | Shows the next race in a panel
nextRace :: Racers -> Html.Html
nextRace = raceBox "Next Race" Nothing

raceBox :: String -> Maybe RaceTime -> Racers -> Html.Html
raceBox ttle t rs =
  BHtml.panelDefault . BHtml.row $
  do BHtml.col "xs-4" $ Html.span ! HtmlA.class_ "h2" $ hstr ttle
     BHtml.col "xs-2" $ Html.span ! HtmlA.class_ "h2" $ Html.toHtml ?< t
     mconcat $ (BHtml.col "xs-3" . showRacer . flip view rs) <$> [_1,_2]
  where
    showRacer r =
      Html.span ! HtmlA.class_ "h2" $
      racerIcon r <^ " " <> (Html.small $ (r ^. racerNameHtml))

racerRank :: Rank -> Racer -> RaceTime -> Html.Html
racerRank rnk r t =
  BHtml.panelDefault . BHtml.row $
  do (colTemp "3" . BHtml.badge . Html.toHtml $ rnk)
     colTemp "3" $ r ^. racerNameHtml
     colTemp "6" $ Html.toHtml t
  where
    colTemp x = BHtml.col ("xs-" ++ x) . (Html.span ! HtmlA.class_ "h2")

matchDataIcon :: MatchData -> Html.Html
matchDataIcon md = BHtml.addPopOver "Match Data" matchContent matchDataHtml
  where
    matchContent =
      Html.div ! HtmlA.class_ "circle" $
      Html.span ! HtmlA.class_ "h1" $ BHtml.glyphicon "flag"
    matchDataHtml =
      mconcat $ (matchH . (flip view md)) <$> [firstRacerData, secondRacerData]
    matchH (r, t, w) =
      BHtml.row $
      do (r ^. racerNameHtml) <^ " "
         w <? (BHtml.glyphicon "king") <^ " "
         Html.toHtml t
