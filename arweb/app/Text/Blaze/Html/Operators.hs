{-|
Module      : Name
Description : Operator extensions to blaze
Copyright   : (c) Some Guy, 2013
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

-}

module Text.Blaze.Html.Operators where

import Text.Blaze.Html
import Data.Bool

-- | Compinator for conditional data
(?<) :: (a -> Html) -> Maybe a -> Html
f ?< m = maybe mempty f $ m

(<?) :: Bool -> Html -> Html
b <? h = bool mempty h b

-- | convert a string to html
(<^) :: Html -> String -> Html
infixl 8 <^ 
h <^ s = mappend h . toHtml $ s
