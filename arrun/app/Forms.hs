{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Forms where

import Control.Lens
import Control.Applicative
import Data.Text (Text(..))
import Happstack.Server
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html
import Text.Reform hiding (view)
import Text.Reform.Blaze.Common (setAttr)
import Text.Reform.Blaze.Text
import Text.Reform.Happstack
import Web.Routes.PathInfo
import qualified Data.Text as Strict

type PasteForm m a = (Happstack m, Monad m, Alternative m, Functor m) =>
                     Form m [Input] () Html () a

instance FormError () where
  type ErrorInputType () = [Input]
  commonFormError _ = ()

type NewRacerFormData = Strict.Text

data UploadCodeFormData = UploadCodeFormData
  { buildName :: Strict.Text
  , buildFile :: FilePath
  }

newRacerForm act handle = reform (form act) "new-racer" handle Nothing genNewRacerForm

uploadCodeForm act handle = reform (form act) "upload-code" handle Nothing genUploadCodeForm

genNewRacerForm :: PasteForm m NewRacerFormData
genNewRacerForm =
  fieldset $ inputText "" `setAttr` A.placeholder "New Racer Name"

genUploadCodeForm :: PasteForm m UploadCodeFormData
genUploadCodeForm =
  fieldset $
  UploadCodeFormData <$> (inputText "" `setAttr` A.placeholder "Build Name") <*>
  (view _1 <$> inputFile)
