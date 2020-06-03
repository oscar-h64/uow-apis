-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Config (
    loadConfig,

    module Warwick.Config
) where 

-------------------------------------------------------------------------------

import Data.Maybe
import Data.Text

import System.Directory
import System.Environment

import Warwick.Config

-------------------------------------------------------------------------------

-- | 'loadConfig' is a computation which tries to load the 
loadConfig :: IO APIConfig 
loadConfig = do 
    exists <- doesFileExist "warwick.json"

    -- first, try to load the configuration from disk 
    mCfg <- if exists then readAPIConfig "warwick.json"
            else pure Nothing

    case mCfg of 
        -- success: return the configuration loaded from disk 
        Just cfg -> pure cfg 
        -- failure: try to read the configuration from environment
        -- variables instead
        Nothing -> do 
            username <- fromMaybe "" <$> lookupEnv "UOW_USER"
            password <- fromMaybe "" <$> lookupEnv "UOW_PASSWORD"

            pure APIConfig{
                apiUsername = pack username,
                apiPassword = pack password
            }

-------------------------------------------------------------------------------
