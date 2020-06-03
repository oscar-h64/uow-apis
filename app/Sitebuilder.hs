-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Sitebuilder ( sitebuilderMain ) where 

-------------------------------------------------------------------------------

import Data.Maybe
import Data.Text

import System.Environment
import System.Exit
import System.IO

import Warwick.Config
import Warwick.Common
import Warwick.Sitebuilder

import CmdArgs 

-------------------------------------------------------------------------------

handleAPI :: Show e => IO (Either e a) -> IO a
handleAPI m = m >>= \case 
    Left err -> do 
        hPutStrLn stderr (show err)
        exitWith (ExitFailure (-1)) 
    Right _ -> exitSuccess

sitebuilderMain :: SitebuilderOpts -> IO ()
sitebuilderMain opts = do 
    username <- fromMaybe "" <$> lookupEnv "SB_USER"
    password <- fromMaybe "" <$> lookupEnv "SB_PASSWORD"

    let config = APIConfig {
        apiUsername = pack username,
        apiPassword = pack password
    }

    case opts of 
        EditPage{..} -> do 
            let comment = fromMaybe "" cComment

            handleAPI $ withAPI Live config $ 
                editPageFromFile cPage comment cFile
        UploadFile{..} -> do 
            let name = fromMaybe "" cSlug

            handleAPI $ withAPI Live config $ 
                uploadFile cPage name cFile

-------------------------------------------------------------------------------
