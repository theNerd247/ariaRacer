{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Forms where

import Control.Lens
import Control.Applicative
import Data.Text (Text(..))
import Happstack.Server
import Text.Blaze.Html5 (Html)
import Text.Blaze.Bootstrap
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
  { ubuildName :: Strict.Text
  , ubuildFile :: FilePath
  } deriving (Show)

newRacerForm act handle = reform (form act) "new-racer" handle Nothing genNewRacerForm

uploadCodeForm act handle = reform (form act) "upload-code" handle Nothing genUploadCodeForm

genNewRacerForm :: PasteForm m NewRacerFormData
genNewRacerForm =
  buttonSubmit "Submit" (glyphicon "plus" <> glyphicon "user")`setAttr` (A.type_ "submit" <> A.class_ "btn btn-success") 
  *> inputText "" `setAttr` (A.placeholder "New Racer Name" <> A.class_ "form-control")

genUploadCodeForm :: PasteForm m UploadCodeFormData
genUploadCodeForm =
  fieldset $ 
    buttonSubmit "Submit" (glyphicon "plus" <> glyphicon "duplicate") `setAttr` (A.type_ "submit" <> A.class_ "btn btn-success") 
    *> pure UploadCodeFormData 
    <*> (inputText "" `setAttr` (A.placeholder "Build Name" <> A.class_ "form-control"))
    <*> (view _1 <$> inputFile) `setAttr` (A.class_ "form-control")
