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
    getMyAddress,
    getMyItems
) where 

-------------------------------------------------------------------------------

import Control.Monad.Trans

import Data.Aeson
-- import Data.Text

-- import Network.HTTP.Conduit

import Servant.Client

import Warwick.Common
import Warwick.Config
import qualified Warwick.Postroom.API as PS
import qualified Warwick.Postroom.OpeningTimes as PS
import qualified Warwick.Postroom.PostItem as PS
import qualified Warwick.Postroom.PostroomHub as PS
import qualified Warwick.Postroom.Recipient as PS

-------------------------------------------------------------------------------

-- | Enumerates Postroom API instances.
data PostroomInstance = Live | CustomInstance BaseUrl
    deriving (Eq, Show)

instance ToJSON PostroomInstance where 
    toJSON Live    = String "live"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON PostroomInstance where 
    parseJSON (String "live")    = pure Live 
    parseJSON val = flip (withObject "PostroomInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | The URL to the Campus API.
liveURL :: BaseUrl
liveURL = BaseUrl Https "postroom.warwick.ac.uk" 443 "/api/"

instance HasBaseUrl PostroomInstance where
    getBaseUrl Live                 = liveURL
    getBaseUrl (CustomInstance url) = url

-------------------------------------------------------------------------------

getHubs :: Warwick [PS.PostroomHub]
getHubs = do
    authData <- getAuthData
    lift $ lift $ PS.getHubs authData

getOpeningTimes :: Warwick PS.OpeningTimes
getOpeningTimes = do
    authData <- getAuthData
    lift $ lift $ PS.getOpeningTimes authData

getMyAddress :: Warwick [PS.Recipient]
getMyAddress = do
    authData <- getAuthData
    lift $ lift $ PS.getMyAddress authData

getMyItems :: Warwick [PS.PostItem]
getMyItems = do
    authData <- getAuthData
    lift $ lift $ PS.getMyItems authData

-------------------------------------------------------------------------------
