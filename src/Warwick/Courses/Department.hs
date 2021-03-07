-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Courses.Department ( Department(..) ) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Courses.Faculty

-------------------------------------------------------------------------------

-- | Represents information about a department.
data Department = MkDepartment {
    -- | The department code, e.g. "CS".
    departmentCode :: Text,
    -- | The full name of the department, e.g. "Computer Science".
    departmentName :: Text,
    -- | The short name of the department, e.g. "DCS".
    departmentShortName :: Text,
    -- | The very short name of the department, e.g. "DCS".
    departmentVeryShortName :: Text,
    -- | Information about the faculty this department belongs to.
    departmentFaculty :: Faculty
} deriving (Eq, Show)

instance FromJSON Department where
    parseJSON = withObject "Department" $ \obj ->
        MkDepartment <$> obj .: "code" 
                     <*> obj .: "name"
                     <*> obj .: "shortName"
                     <*> obj .: "veryShortName"
                     <*> obj .: "faculty"

instance ToJSON Department where
    toJSON MkDepartment{..} =
        object [ "code" .= departmentCode
               , "name" .= departmentName
               , "shortName" .= departmentShortName
               , "veryShortName" .= departmentVeryShortName
               , "faculty" .= departmentFaculty 
               ]

-------------------------------------------------------------------------------
