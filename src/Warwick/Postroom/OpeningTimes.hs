-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Postroom.OpeningTimes (
    OpeningTimes(..),
) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text ( Text )

-------------------------------------------------------------------------------

-- | Represents the response from the opening times endpoint
data OpeningTimes = OpeningTimes {
    -- | The opening times and linked accommodations of each hub in HTML format
    otHTML :: Text,
    -- | The opening times and linked accommodations of each hub in text format
    otText :: Text
} deriving (Eq, Show)

instance FromJSON OpeningTimes where
    parseJSON = withObject "OpeningTimes" $ \v ->
        OpeningTimes <$> v .: "html"
                     <*> v .: "text"

-------------------------------------------------------------------------------
