module Data.Acid.Run where

import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Maybe (fromMaybe)
import Data.SafeCopy
import System.FilePath ((</>))
import Data.Data
import Control.Exception

withAcid
  :: (IsAcidic s, SafeCopy s, Typeable s)
  => Maybe FilePath -- ^ state directory
  -> s -- ^ initial internal state held by Acid
  -> ((AcidState s) -> IO a) -- ^ action
  -> IO a
withAcid mBasePath initState f =
  let basePath = fromMaybe "_state" mBasePath
  in bracket
       (openLocalStateFrom (basePath </> "ariaRacer") initState)
       (createCheckpointAndClose)
       f
