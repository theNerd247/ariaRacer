{-# LANGUAGE TemplateHaskell #-}

module Pages.Types
  ( Navbar(..)
  , BodyHtml(..)
  , Page(..)
  , pageTitle
  , pageNavbar
  , pageBody
  , RaceTime(..)
  , Racer(..)
  , Rank(..)
  , hstr
  ) where

import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA
import qualified Text.Blaze.Bootstrap as BHtml
import Control.Lens
import Data.Text
import Data.Time
import Data.Fixed

type Navbar = Html.Html

type BodyHtml = Html.Html

data Page = Page
  { _pageTitle :: Text
  , _pageNavbar :: Maybe Navbar
  , _pageBody :: BodyHtml
  }

newtype RaceTime = RaceTime TimeOfDay 

newtype Racer = Racer String

newtype Rank = Rank Integer

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
  toMarkup (Racer n) = Html.span $ do 
    Html.p $ BHtml.addPopOver (BHtml.glyphicon "user") (hstr n)
    

instance Html.ToMarkup Rank where
  toMarkup (Rank n) = hstr $ show n

hstr :: String -> Html.Html
hstr = Html.toHtml
