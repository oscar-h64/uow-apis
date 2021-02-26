-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Tabula.Config (
    TabulaInstance(..),
    liveURL,
    sandboxURL,
    devURL
) where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Servant.API
import Servant.Client

import Warwick.Common

-------------------------------------------------------------------------------

-- | Enumerates Tabula instances.
data TabulaInstance = Sandbox | Live | Dev | CustomInstance BaseUrl
    deriving (Eq, Show)

instance ToJSON TabulaInstance where 
    toJSON Sandbox = String "sandbox"
    toJSON Live    = String "live"
    toJSON Dev     = String "dev"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON TabulaInstance where 
    parseJSON (String "sandbox") = pure Sandbox 
    parseJSON (String "live")    = pure Live
    parseJSON (String "dev")     = pure Dev  
    parseJSON val = flip (withObject "TabulaInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

instance FromHttpApiData TabulaInstance where
    parseQueryParam "live"    = pure Live
    parseQueryParam "dev"     = pure Dev
    parseQueryParam "sandbox" = pure Sandbox
    parseQueryParam url = either (Left . pack . show) (pure . CustomInstance) $
        parseBaseUrl $ unpack url


-- | The URL to the Tabula API.
liveURL :: BaseUrl
liveURL = BaseUrl Https "tabula.warwick.ac.uk" 443 "/api/v1"

-- | The URL to the Tabula Sandbox API.
sandboxURL :: BaseUrl
sandboxURL = BaseUrl Https "tabula-sandbox.warwick.ac.uk" 443 "/api/v1"

-- | The URL to the Tabula-dev API.
devURL :: BaseUrl
devURL = BaseUrl Https "tabula-dev.warwick.ac.uk" 443 "/api/v1"

instance HasBaseUrl TabulaInstance where
    getBaseUrl Sandbox              = sandboxURL
    getBaseUrl Live                 = liveURL
    getBaseUrl Dev                  = devURL
    getBaseUrl (CustomInstance url) = url

--------------------------------------------------------------------------------
