-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Postroom (
    -- module Warwick.Campus.Room,

    PostroomInstance(..),

    -- listRooms
) where 

-------------------------------------------------------------------------------

import Data.Aeson
-- import Data.Text

-- import Network.HTTP.Conduit

import Servant.Client

import Warwick.Common

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

-- | 'listRooms' @limit query@ searches for rooms with names matching 
-- @query@. At most @limit@-many results are returned.
-- listRooms :: Int -> Text -> Warwick [Room]
-- listRooms limit query = do 
--     token <- ask 
--     lift $ lift $ API.listRooms 
--         (Just $ "Token " <> token) (Just 2) (Just limit) (Just query) (Just False)

-------------------------------------------------------------------------------
