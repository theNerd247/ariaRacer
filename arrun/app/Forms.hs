{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Forms where

import Control.Lens
import Control.Applicative
import Aria.Types
import Data.Text (Text(..))
import Happstack.Server
import Text.Blaze.Html5 (Html)
import Text.Blaze.Bootstrap
import Text.Blaze.Html
import Text.Reform hiding (view)
import Text.Reform.Blaze.Common (setAttr)
import Text.Reform.Blaze.Text
import Text.Reform.Happstack
import Web.Routes.PathInfo
import qualified Text.Reform.Generalized as G
import qualified Text.Blaze.Html5 as H
import qualified Data.List as DL
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as Strict

type PasteForm m a = (Happstack m, Monad m, Alternative m, Functor m) =>
                     Form m [Input] PasteFormError Html () a

data PasteFormError
  = WrongFileType
  | BuildNameRequired
  | CFE (CommonFormError [Input])
  deriving (Show)

type NewRacerFormData = Strict.Text

data UploadCodeFormData = UploadCodeFormData
  { ubuildName :: Strict.Text
  , ubuildTmpFile :: FilePath
  } deriving (Show)

type SetupRaceFormData = (RacerId,RacerId) 

instance FormError PasteFormError where
  type ErrorInputType PasteFormError = [Input]
  commonFormError = CFE

instance ToMarkup PasteFormError where
  toMarkup WrongFileType = H.string "You need to upload a .cpp file"
  toMarkup BuildNameRequired = H.string "Build name required"
  toMarkup (CFE e) = H.string "Something went wrong"

newRacerForm act handle = reform (form act) "new-racer" handle Nothing genNewRacerForm

uploadCodeForm act handle = reform (form act) "upload-code" handle Nothing genUploadCodeForm

setupRaceForm racers act handle = reform (form act) "setup-race" handle Nothing (genSetupRaceForm racers)

genNewRacerForm :: PasteForm m NewRacerFormData
genNewRacerForm =
  buttonSubmit "Submit" (glyphicon "plus" <> glyphicon "user") `setAttr`
  (A.type_ "submit" <> A.class_ "btn btn-success") *>
  inputText "" `setAttr`
  (A.placeholder "New Racer Name" <> A.class_ "form-control")

genUploadCodeForm :: PasteForm m UploadCodeFormData
genUploadCodeForm = fieldset $ bootstrapError ++> (submitButton *> uploadForm)
  where
    uploadForm = pure UploadCodeFormData <*> buildName <*> buildFile
    buildFile =
      (view _1 <$> inputFile) `transformEither` cppFileProof `setAttr`
      (A.class_ "form-control")
    submitButton =
      buttonSubmit "Submit" (glyphicon "plus" <> glyphicon "duplicate") `setAttr`
      (A.type_ "submit" <> A.class_ "btn btn-success")
    buildName =
      inputText "" `transformEither` buildNameProof `setAttr`
      (A.placeholder "Build Name" <> A.class_ "form-control")

genSetupRaceForm :: [Racer] -> PasteForm m SetupRaceFormData
genSetupRaceForm racers = bootstrapError ++> (submitButton *> selForm)
  where
    selForm = pure (,) <*> selRacer <*> selRacer
    selRacer = select selLabels defaultRacer `setAttr` (A.class_ "form-control")
    selLabels = (\r -> (r ^. racerId, r ^. racerName)) <$> racers
    defaultRacer = (==((head racers) ^. racerId))
    submitButton = buttonSubmit "Submit" (H.string "Setup Race") `setAttr` (A.type_ "submit" <> A.class_ "btn btn-success")

bootstrapError
  :: (Monad m)
  => PasteForm m ()
bootstrapError = G.childErrors showErrors
  where
    showErrors [] = mempty
    showErrors errs = mconcat $ alertBox BootAlertDanger . toHtml <$> errs

buildNameProof :: Strict.Text -> Either PasteFormError Strict.Text
buildNameProof t =
  case Strict.null . Strict.strip $ t of
    True -> Left BuildNameRequired
    _ -> Right t

cppFileProof :: FilePath -> Either PasteFormError FilePath
cppFileProof f =
  case (DL.isSuffixOf ".cpp" f) of
    True -> Right f
    _ -> Left WrongFileType
