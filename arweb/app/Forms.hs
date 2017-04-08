{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Forms where

import Control.Lens
import Control.Applicative
import Aria.Types
import Data.Text (Text(..))
import Data.Char (isNumber,digitToInt)
import Data.Monoid
import Control.Monad
import Data.Maybe (isJust)
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

type AriaForm m a = Form m [Input] AriaFormError Html () a

data AriaFormError
  = WrongFileType
  | BuildNameRequired
  | BadRaceSelect
  | InvalidIp
  | CFE (CommonFormError [Input])
  deriving (Show)

type NewRacerFormData = Strict.Text

data UploadCodeFormData = UploadCodeFormData
  { ubuildName :: Strict.Text
  , ubuildTmpFile :: FilePath
  } deriving (Show)

type IpAddressForm = (Strict.Text,Strict.Text,Strict.Text,Strict.Text)

type RobotIpFormData = (Maybe String,Maybe String)

type SetupRaceFormData = (Maybe RacerId,Maybe RacerId) 

instance FormError AriaFormError where
  type ErrorInputType AriaFormError = [Input]
  commonFormError = CFE

instance ToMarkup AriaFormError where
  toMarkup WrongFileType = H.string "You need to upload a .cpp file"
  toMarkup BuildNameRequired = H.string "Build name required"
  toMarkup (CFE e) = H.string "Something went wrong"
  toMarkup BadRaceSelect = H.string "You need to select at least one racer to race with"
  toMarkup InvalidIp = H.string "An Ip address is invalid!"

newRacerForm act handle = reform (form act) "new-racer" handle Nothing genNewRacerForm

uploadCodeForm act handle = reform (form act) "upload-code" handle Nothing genUploadCodeForm

setupRaceForm racers act handle = reform (form act) "setup-race" handle Nothing (genSetupRaceForm racers)

robotIpForm act handle = reform (form act) "robot-ips" handle Nothing (genRobotIpForm)

genNewRacerForm :: (Happstack m, Monad m, Alternative m, Functor m) => AriaForm m NewRacerFormData
genNewRacerForm = mkInline $ 
  buttonSubmit "Submit" (glyphicon "plus" <> glyphicon "user") `setAttr`
  (A.type_ "submit" <> A.class_ "btn btn-success") *>
  inputText "" `setAttr`
  (A.placeholder "New Racer Name" <> A.class_ "form-control")

genUploadCodeForm :: (Happstack m, Monad m, Alternative m, Functor m) => AriaForm m UploadCodeFormData
genUploadCodeForm = mkInline . fieldset $ bootstrapError ++> (submitButton *> uploadForm)
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

genSetupRaceForm :: (Happstack m, Monad m, Alternative m, Functor m) => [Racer] -> AriaForm m SetupRaceFormData
genSetupRaceForm racers = bootstrapError ++> (selForm `transformEither` setupRaceProof <* submitButton)
  where
    selForm = pure (,) 
      <*> (mkFormGroup $ label ("Lane 1" :: String) ++> selRacer) 
      <*> (mkFormGroup $ label ("Lane 2" :: String) ++> selRacer) 
    selRacer = select (noRacerLabel:selLabels) defaultRacer `setAttr` (A.class_ "form-control")
    selLabels = fmap (\r -> (Just $ r ^. racerId, racerLabel r)) racers
    noRacerLabel = (Nothing,"No Racer Selected")
    racerLabel r = (H.string $ r ^. racerId . unRacerId. to show) <> " - " <> (H.text $ r ^. racerName)
    defaultRacer = (==Nothing)
    submitButton = buttonSubmit "Submit" (H.string "Setup Race") `setAttr` (A.type_ "submit" <> A.class_ "btn btn-success")

genRobotIpForm :: (Happstack m, Monad m, Alternative m, Functor m) => AriaForm m RobotIpFormData
genRobotIpForm = mkInline $ bootstrapError ++> (ipForms `transformEither` ipAddressProof <* submitButton)
  where
    ipForms = (,) <$> (label ("Lane 1 Robot Ip" :: String) ++> ipForm) <*> (label ("Lane 2 Robot Ip" :: String) ++> ipForm)
    ipForm = (,,,) <$> ipNum <*> ipNum <*> ipNum <*> ipNum
    ipNum = inputText "0" `setAttr` (A.size "3")
    submitButton = buttonSubmit "Submit" (H.string "Set Ips") `setAttr` (A.type_ "submit" <> A.class_ "btn btn-success")

setupRaceProof :: SetupRaceFormData -> Either AriaFormError SetupRaceFormData
setupRaceProof (Nothing,Nothing) = Left BadRaceSelect
setupRaceProof x = Right x

mkFormGroup :: (Happstack m, Monad m, Alternative m, Functor m) => AriaForm m a -> AriaForm m a
mkFormGroup = mapView $ H.div ! A.class_ "form-group" 

mkInline :: (Happstack m, Monad m, Alternative m, Functor m) => AriaForm m a -> AriaForm m a
mkInline = mapView $ \v -> v ! A.class_ "form-inline"

bootstrapError
  :: (Monad m)
  => AriaForm m ()
bootstrapError = G.childErrors showErrors
  where
    showErrors [] = mempty
    showErrors errs = mconcat $ alertBox BootAlertDanger . toHtml <$> errs

buildNameProof :: Strict.Text -> Either AriaFormError Strict.Text
buildNameProof t =
  case Strict.null . Strict.strip $ t of
    True -> Left BuildNameRequired
    _ -> Right t

cppFileProof :: FilePath -> Either AriaFormError FilePath
cppFileProof f =
  case (DL.isSuffixOf ".cpp" f) of
    True -> Right f
    _ -> Left WrongFileType

ipAddressProof :: (IpAddressForm,IpAddressForm) -> Either AriaFormError RobotIpFormData
ipAddressProof (f1,f2) = (,) <$> (checkForm f1) <*> (checkForm f2)
  where
    checkForm ip@(a,b,c,d) = do 
      let ds = [a,b,c,d]
      allOk <- (getAll . mconcat . fmap (All . isJust)) <$> forM ds checkNum
      case allOk of
        True -> Right . Just $ DL.intercalate "." $ Strict.unpack <$> ds
        False -> Right Nothing

    checkNum x 
      | Strict.null x == True = Right Nothing
      | otherwise = do 
          let xs = Strict.unpack x
          ns <- case (getAll . mconcat . fmap (All . isNumber) $ xs) of
            True -> Right (digitToInt <$> xs)
            _ -> Left InvalidIp
          checkValidIpNum ns
          return $ Just x

    checkValidIpNum [] = Left InvalidIp
    checkValidIpNum xs 
      | 0 <= (toNum xs) && (toNum xs) <= 255 = Right xs
      | otherwise = Left InvalidIp

    toNum xs = sum . zipWith (\a n -> a*(10^n)) xs $ reverse [0..(length xs - 1)]
