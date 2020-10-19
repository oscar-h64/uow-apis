-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Postroom.PostroomHub (
    PostroomHub(..),
) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text ( Text )
import Data.UUID.Types ( UUID )

-------------------------------------------------------------------------------

-- | Represents a Warwick postroom hub
data PostroomHub = PostroomHub {
    -- | The unique ID of the hub
    phId :: UUID,
    -- | The name of the hub
    phName :: Text,
    -- | Where the hub is on campus
    phLocation :: Text,
    -- | The ID of the location on the campus map
    phMapId :: Text
} deriving (Eq, Show)

instance FromJSON PostroomHub where
    parseJSON = withObject "PostroomHub" $ \v ->
        PostroomHub <$> v .: "id"
                    <*> v .: "name"
                    <*> v .: "location"
                    <*> v .: "mapId"

-------------------------------------------------------------------------------
