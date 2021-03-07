-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Courses.Leader ( Leader(..) ) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

-------------------------------------------------------------------------------

-- | Represents information about a module leader.
data Leader = MkLeader {
    -- | The University ID of the module leader.
    leaderUniversityId :: Text,
    -- | The name of the module leader.
    leaderName :: Text,
    -- | The email address of the module leader.
    leaderEmail :: Text
} deriving (Eq, Show)

instance FromJSON Leader where
    parseJSON = withObject "Leader" $ \obj ->
        MkLeader <$> obj .: "universityId" 
                 <*> obj .: "name" 
                 <*> obj .: "email"

instance ToJSON Leader where
    toJSON MkLeader{..} =
        object [ "universityId" .= leaderUniversityId
               , "name" .= leaderName
               , "email" .= leaderEmail
               ]

-------------------------------------------------------------------------------
