-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Courses.Faculty ( Faculty(..) ) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

-------------------------------------------------------------------------------

-- | Represents information about a faculty.
data Faculty = MkFaculty {
    -- | The faculty code.
    facultyCode :: Text,
    -- | The full name of the faculty.
    facultyName :: Text
} deriving (Eq, Show)

instance FromJSON Faculty where
    parseJSON = withObject "Faculty" $ \obj ->
        MkFaculty <$> obj .: "code" <*> obj .: "name"

instance ToJSON Faculty where
    toJSON MkFaculty{..} =
        object [ "code" .= facultyCode, "name" .= facultyName ]

-------------------------------------------------------------------------------
