-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Sitebuilder ( sitebuilderMain ) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Yaml (decodeFileThrow)

import System.Exit
import System.IO

import Warwick.Config
import Warwick.Common
import Warwick.Sitebuilder
import Warwick.Sitebuilder.PageOptions

import CmdArgs 

-------------------------------------------------------------------------------

data PageConfig = PageConfig {
    scPage :: Text,
    scContent :: FilePath,
    scProperties :: PageOptions,
    scFiles :: FilePath,
    scChildren :: [PageConfig]
}

instance FromJSON PageConfig where
    parseJSON = withObject "PageConfig" $ \v -> 
        PageConfig <$> v .: "page"
                   <*> v .: "content"
                   <*> v .: "properties"
                   <*> v .: "files"
                   <*> v .: "children"

-------------------------------------------------------------------------------

handleAPI :: Show e => IO (Either e a) -> IO a
handleAPI m = m >>= \case 
    Left err -> do 
        hPutStrLn stderr (show err)
        exitWith (ExitFailure (-1)) 
    Right _ -> exitSuccess

processPage :: APIConfig -> PageConfig -> IO ()
processPage = undefined

sitebuilderMain :: APIConfig -> SitebuilderOpts -> IO ()
sitebuilderMain config opts = do 

    case opts of 
        EditPage{..} -> do 
            let comment = fromMaybe "" cComment

            handleAPI $ withAPI Live config $ 
                editPageFromFile cPage comment cFile
        UploadFile{..} -> do 
            let name = fromMaybe "" cSlug

            handleAPI $ withAPI Live config $ 
                uploadFile cPage name cFile
        SyncSite{..} -> do
            conf <- decodeFileThrow cConfigPath :: IO [PageConfig]

            mapM_ (processPage config) conf

-------------------------------------------------------------------------------
