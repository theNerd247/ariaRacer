{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Aria.Repo
import Aria.Repo.DB
import Aria.Types
import Data.SafeCopy
import Data.Data
import Data.Acid.Run
import Data.Time (UTCTime(..))
import Data.Serialize.Put
import Data.List (intersperse)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State
import Control.Lens
import Happstack.Server
import Web.Routes
import Web.Routes.Happstack
import Control.Monad.Catch
import Text.Blaze ((!), string)
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Aria.Scripts as AS

type ARRunApp = RouteT Route (RepoApp (ServerPartT IO))

data Route
  = NewRacer Racer
  | DelRacer RacerId
  | BuildRacer RacerId
               CodeRevision
  | ScriptLogs
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance PathInfo RacerId

instance PathInfo Racer

instance PathInfo Route

instance ToMessage Racer where
  toMessage = runPutLazy . safePut

instance ToMessage RacerId where
  toMessage = runPutLazy . safePut

instance (ToMessage a, SafeCopy a) =>
         ToMessage [a] where
  toMessage = mconcat . fmap (runPutLazy . safePut)

instance ToMessage AS.ScriptLogData where
  toMessage = runPutLazy . safePut

instance H.ToMarkup AS.ScriptLog where
  toMarkup = H.toHtml . fmap H.toHtml

instance H.ToMarkup UTCTime where
  toMarkup = H.toHtml . show

instance H.ToMarkup AS.ScriptLogData where
  toMarkup logData =
    H.div ! A.class_ "arscript-log-data" $
    do H.div ! A.class_ "arscript-command" $
         do H.span ! A.class_ "cmd" $ H.toHtml . show $ (logData ^. scriptCmd)
            " "
            H.span ! A.class_ "exitcode" $
              H.toHtml . show $ (logData ^. exitCode)
            " "
            H.span ! A.class_ "file" $ H.toHtml . show $ (logData ^. scriptFile)
            " "
            H.span ! A.class_ "args" $
              mconcat . fmap H.toHtml . intersperse " " $
              (logData ^. scriptArgs)
       H.div ! A.class_ "arscript-rundata" $
         do H.div ! A.class_ "arscript-runtimes" $
              do H.span ! A.class_ "startTime" $
                   H.toHtml $ (logData ^. scriptStartTime)
                 " - "
                 H.span ! A.class_ "endTime" $
                   H.toHtml $ (logData ^. scriptEndTime)
            H.div ! A.class_ "arscript-pipes" $
              do H.div ! A.class_ "stdout" $
                   do H.span $ "stdout" <> H.br <> "---------"
                      H.pre $ H.toHtml $ (logData ^. stdOut)
                 H.div ! A.class_ "stderr" $
                   do H.span $ "stderr" <> H.br <> "---------"
                      H.pre $ H.toHtml $ (logData ^. stdErr)

newRacerHndl :: Racer -> ARRunApp Response
newRacerHndl r = lift $ newRacer r >>= ok . toResponse 

showScriptLogs :: ARRunApp Response
showScriptLogs = lift getScriptLogs >>= ok . toResponse . H.toHtml

removeRacerHndl :: RacerId -> ARRunApp Response
removeRacerHndl rid = lift $ deleteRacer rid >>= ok . toResponse

buildRacerHndl :: RacerId -> CodeRevision -> ARRunApp Response
buildRacerHndl rid rev = lift $ buildRacer rid rev >>= ok . toResponse 

route :: Route -> ARRunApp Response
route r = do
  case r of
    (NewRacer r) -> newRacerHndl r
    (DelRacer rid) -> removeRacerHndl rid
    (BuildRacer rid rev) -> buildRacerHndl rid rev
    ScriptLogs -> showScriptLogs
  `catch`
    (\(ScriptError log) -> ok . toResponse $ log)


initRepo :: RepoDBState
initRepo =
  RepoDBState
  { _racerDB = emptyRacerDB
  , _nextRacerId = RacerId 1
  , _scriptLog = []
  , _scriptConfig =
    AS.ScriptConfig
    { AS._scriptBasePath = "/home/noah/src/com/ariaRacer/scripts"
    , AS._scriptCwd = "/tmp/arrun"
    }
  }

runRoutes :: RepoAppState -> Site Route (ServerPartT IO Response)
runRoutes initState =
  mkSitePI $
  \showFun url -> flip evalStateT initState $ runRouteT route showFun url

main = do
  withAcid (Just "/tmp/_state") initRepo $
    (simpleHTTP nullConf . implSite "" "" . runRoutes)
