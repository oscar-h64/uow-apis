-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Postroom (
    module Warwick.Config,
    module Warwick.Common,

    PostroomInstance(..),

    getHubs,
    getOpeningTimes,
    getMyAddresses,
    getMyItems
) where 

-------------------------------------------------------------------------------

import Control.Monad.Trans

import Data.Aeson

import Servant.Client

import Warwick.Common
import Warwick.Config
import qualified Warwick.Postroom.API as PR
import qualified Warwick.Postroom.OpeningTimes as PR
import qualified Warwick.Postroom.PostItem as PR
import qualified Warwick.Postroom.PostroomHub as PR
import qualified Warwick.Postroom.Recipient as PR

-------------------------------------------------------------------------------

-- | Enumerates Postroom API instances.
data PostroomInstance = Live | Dev | CustomInstance BaseUrl
    deriving (Eq, Show)

instance ToJSON PostroomInstance where 
    toJSON Live = String "live"
    toJSON Dev  = String "dev"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON PostroomInstance where 
    parseJSON (String "live") = pure Live
    parseJSON (String "dev")  = pure Dev
    parseJSON val = flip (withObject "PostroomInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | The URL to the live Postroom instance.
liveURL :: BaseUrl
liveURL = BaseUrl Https "postroom.warwick.ac.uk" 443 "/api/"

-- | The URL to the dev Postroom instance.
devURL :: BaseUrl
devURL = BaseUrl Https "postroom-dev.warwick.ac.uk" 443 "/api/"

instance HasBaseUrl PostroomInstance where
    getBaseUrl Live                 = liveURL
    getBaseUrl Dev                  = devURL
    getBaseUrl (CustomInstance url) = url

-------------------------------------------------------------------------------

-- | `getHubs` retrieves basic details about all postroom hubs
getHubs :: Warwick [PR.PostroomHub]
getHubs = do
    authData <- getAuthData
    lift $ lift $ PR.getHubs authData

-- | `getOpeningTimes` retrieves the opening times for every postroom hub. It
-- also includes which accommodations use each hub
getOpeningTimes :: Warwick PR.OpeningTimes
getOpeningTimes = do
    authData <- getAuthData
    lift $ lift $ PR.getOpeningTimes authData

-- | `getMyAddresses` retrieves the active addresses for the current user
getMyAddresses :: Warwick [PR.Recipient]
getMyAddresses = do
    authData <- getAuthData
    lift $ lift $ PR.getMyAddresses authData

-- | `getMyItems` retrieves all post items for the current user
getMyItems :: Warwick [PR.PostItem]
getMyItems = do
    authData <- getAuthData
    lift $ lift $ PR.getMyItems authData

-------------------------------------------------------------------------------
