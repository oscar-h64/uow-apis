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

import System.Directory (doesFileExist, doesDirectoryExist)
import System.Exit
import System.IO

import Warwick.Config
import Warwick.Common
import Warwick.Sitebuilder
import Warwick.Sitebuilder.PageOptions

import CmdArgs 

-------------------------------------------------------------------------------

data PageConfig = PageConfig {
    -- | The path of the page on the site
    pcPage :: Text,
    -- | The path to the contents for the page
    pcContent :: FilePath,
    -- | The properties for the page
    pcProperties :: PageOptions,
    -- | The files to upload under this page. If a directory is specified all
    -- files in that directory will be uploaded. If the file or folder does
    -- not exist the program simply continues
    pcFiles :: [FilePath],
    -- | The children of the page
    pcChildren :: [PageConfig]
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

processPage :: APIConfig -> Text -> PageConfig -> IO ()
processPage config prefix PageConfig{..} = do
    -- TODO: this breaks if prefix doesn't end in /
    let page = prefix <> pcPage

    -- upload page
    -- TODO: need to check if page exists and use create if so
    handleAPI $ withAPI Live config $ editPageFromFile page "" pcContent

    -- upload files

    -- process children
    mapM_ (processPage config page) pcChildren

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
            pages <- decodeFileThrow cConfigPath :: IO [PageConfig]

            mapM_ (processPage config "") pages

-------------------------------------------------------------------------------
