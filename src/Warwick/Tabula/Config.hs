--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Config (TabulaConfig(..), readTabulaConfig) where

--------------------------------------------------------------------------------

import GHC.Generics

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Text

--------------------------------------------------------------------------------

data TabulaConfig = TabulaConfig {
    tabulaUsername :: Text,
    tabulaPassword :: Text
} deriving (Generic)

instance FromJSON TabulaConfig where
    parseJSON = withObject "TabulaConfig" $ \v ->
        TabulaConfig <$> v .: "username"
                     <*> v .: "password"

-- | `readTabulaConfig` @fp reads the configuration for the Tabula client
-- from a file located at @fp.
readTabulaConfig :: FilePath -> IO (Maybe TabulaConfig)
readTabulaConfig fp = decode <$> BS.readFile fp

--------------------------------------------------------------------------------
