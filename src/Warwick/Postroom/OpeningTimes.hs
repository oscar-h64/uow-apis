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

data OpeningTimes = OpeningTimes {
    otHTML :: Text,
    otText :: Text
}

instance FromJSON OpeningTimes where
    parseJSON = withObject "OpeningTimes" $ \v ->
        OpeningTimes <$> v .: "html"
                     <*> v .: "text"

-------------------------------------------------------------------------------
