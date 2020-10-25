-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Sitebuilder ( sitebuilderMain ) where 

-------------------------------------------------------------------------------

import Control.Monad (forM_)

import Data.Aeson
import Data.Maybe
import Data.Text as T (Text, breakOnEnd, last, pack, snoc)
import Data.Yaml (decodeFileThrow)

import System.Exit
import System.FilePattern (FilePattern)
import System.FilePattern.Directory (getDirectoryFiles) 
import System.IO

import Warwick.Config
import Warwick.Common
import Warwick.Sitebuilder
import Warwick.Sitebuilder.PageOptions (PageOptions, defaultPageOpts)

import CmdArgs 

-------------------------------------------------------------------------------

-- | The configuration for syncing a particular page
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
                   <*> fmap (fromMaybe defaultPageOpts) (v .:? "properties")
                   <*> fmap (fromMaybe []) (v .:? "files")
                   <*> fmap (fromMaybe []) (v .:? "children")

-------------------------------------------------------------------------------

-- | `processPage` @apiCfg parent pageCfg@ syncs the page described by
-- @pageCfg@ with sitebuilder, using @parent@ as the URL of the parent and
-- @apiCfg@ to authenticate
processPage :: APIConfig -> Text -> PageConfig -> IO ()
processPage apiCfg parent PageConfig{..} = do
    -- generate path for this page
    let page = parent <> pcPage

    -- check whether the page exists by checking if getting the page info
    -- throws an error
    info <- withAPI Live apiCfg $ pageInfo page

    -- create the page if it doesn't exist or edit it if it does
    -- TODO: Sort page title
    let (pageParent, pageName) = case breakOnEnd "/" page of
            (pageNoSlash, "") -> breakOnEnd "/" pageNoSlash
            x -> x
    case info of
        Left _ -> handleAPI $ withAPI Live apiCfg
                            $ createPageFromFile pageParent "" pageName pcContent
        Right _ -> handleAPI $ withAPI Live apiCfg
                             $ editPageFromFile page "" pcContent           

    -- get all files matching the patterns given and upload them
    files <- getDirectoryFiles "." pcFiles
    forM_ files $ \f -> handleAPI $ withAPI Live apiCfg
                                  $ uploadFile page (pack f) f

    -- process children
    let newParent = if T.last page == '/' then page else page `snoc` '/'
    mapM_ (processPage apiCfg newParent) pcChildren

-------------------------------------------------------------------------------

handleAPI :: Show e => IO (Either e a) -> IO ()
handleAPI m = m >>= \case 
    Left err -> do 
        hPutStrLn stderr (show err)
        exitWith (ExitFailure (-1)) 
    Right _ -> pure ()

sitebuilderMain :: APIConfig -> SitebuilderOpts -> IO ()
sitebuilderMain config opts = do 

    case opts of 
        EditPage{..} -> do 
            let comment = fromMaybe "" cComment

            handleAPI $ withAPI Live config $ 
                editPageFromFile cPage comment cFile
        UploadFile{..} -> do 
            let name = fromMaybe (pack cFile) cSlug

            handleAPI $ withAPI Live config $ 
                uploadFile cPage name cFile
        SyncSite{..} -> do
            -- read the sync config from the given filepath
            pages <- decodeFileThrow cConfigPath :: IO [PageConfig]

            -- process every page specified in the config (children are
            -- processed recursively by `processPage`)
            mapM_ (processPage config "") pages

-------------------------------------------------------------------------------
