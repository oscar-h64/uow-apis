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
import Data.Text as T (Text, snoc, last)
import Data.Yaml (decodeFileThrow)

import System.Exit
import System.FilePattern (FilePattern)
import System.FilePattern.Directory (getDirectoryFiles) 
import System.IO

import Warwick.Config
import Warwick.Common
import Warwick.Sitebuilder
import Warwick.Sitebuilder.PageOptions ( PageOptions )

import CmdArgs 

-------------------------------------------------------------------------------

data PageConfig = PageConfig {
    -- | The path of the page on the site
    pcPage :: Text,
    -- | The path to the contents for the page
    pcContent :: FilePath,
    -- | The properties for the page
    pcProperties :: PageOptions,
    -- | The files to upload under this page. Note that all files are flattened
    -- and uploaded directly under the page, directory structure is not kept
    pcFiles :: [FilePattern],
    -- | The children of the page
    pcChildren :: [PageConfig]
} deriving (Eq, Show)

instance FromJSON PageConfig where
    parseJSON = withObject "PageConfig" $ \v -> 
        PageConfig <$> v .: "page"
                   <*> v .: "content"
                   <*> v .: "properties"
                   <*> fmap (fromMaybe []) (v .:? "files")
                   <*> fmap (fromMaybe []) (v .:? "children")

-------------------------------------------------------------------------------

handleAPI :: Show e => IO (Either e a) -> IO a
handleAPI m = m >>= \case 
    Left err -> do 
        hPutStrLn stderr (show err)
        exitWith (ExitFailure (-1)) 
    Right _ -> exitSuccess

processPage :: APIConfig -> Text -> PageConfig -> IO ()
processPage config parent PageConfig{..} = do
    -- generate path for this page by appending it to the parent page, adding
    -- a '/' if necessary
    let page = if T.last parent == '/'
               then parent
               else parent `snoc` '/' <> pcPage

    -- check whether the page exists by checking if getting the page info
    -- throws an error
    info <- withAPI Live config $ pageInfo page

    -- create the page if it doesn't exist or edit it if it does
    -- TODO: Sort page title
    let action = case info of
            Left _ -> flip createPageFromFile ""
            Right _ -> editPageFromFile            
    handleAPI $ withAPI Live config $ action page "" pcContent

    -- get all files matching the patterns given and upload them
    files <- getDirectoryFiles "." pcFiles
    mapM_ (\f -> handleAPI $ withAPI Live config $ uploadFile page "" f) files

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
