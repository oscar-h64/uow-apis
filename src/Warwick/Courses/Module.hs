-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Courses.Module ( Module(..) ) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Courses.Faculty
import Warwick.Courses.Department
import Warwick.Courses.Leader

-------------------------------------------------------------------------------

-- | Represents information about a module as obtained from the 
-- course catalogue.
data Module = MkModule {
    -- | The module code, excluding credits, e.g. "CS141".
    moduleCode :: Text,
    -- | The full module code including credits, e.g. "CS141-15".
    moduleCodeWithCATs :: Text,
    -- | The academic year in the "20/21" format.
    moduleAcademicYear :: Text,
    -- | A list of academic years in the "20/21" format in which this module
    -- is available.
    moduleAvailableYears :: [Text],
    -- | The name of the module.
    moduleTitle :: Text,
    -- | The number of credits for this module.
    moduleCreditValue :: Int,
    -- | Always `False`?
    moduleHideFromPublic :: Bool,
    -- | Information about the department to which this module belongs.
    moduleDepartment :: Department,
    -- | A description of the module duration.
    moduleDuration :: Text,
    -- | Information about the faculty to which this module belongs.
    moduleFaculty :: Faculty,
    -- | Information about the module leader.
    moduleLeader :: Leader
    -- TODO: there are a lot more fields available
} deriving (Eq, Show)

instance FromJSON Module where
    parseJSON = withObject "Module" $ \obj ->
        MkModule <$> obj .: "stemCode"
                 <*> obj .: "code"
                 <*> obj .: "academicYear"
                 <*> obj .: "availableAcademicYears"
                 <*> obj .: "title"
                 <*> obj .: "creditValue"
                 <*> obj .: "hideFromPublic"
                 <*> obj .: "department"
                 <*> obj .: "duration"
                 <*> obj .: "faculty"
                 <*> obj .: "leader"

-------------------------------------------------------------------------------
