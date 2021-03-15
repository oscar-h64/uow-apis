-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Courses.ModuleResult ( ModuleResult(..) ) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

-------------------------------------------------------------------------------

data ModuleResult = MkModuleResult {
    moduleResultCode :: Text,
    moduleResultTitle :: Text,
    moduleResultDepartmentCode :: Text,
    moduleResultAcademicYear :: Int
} deriving (Eq, Show)

instance FromJSON ModuleResult where
    parseJSON = withObject "ModuleResult" $ \obj ->
        MkModuleResult <$> obj .: "code"
                       <*> obj .: "title"
                       <*> obj .: "departmentCode"
                       <*> obj .: "academicYear"

-------------------------------------------------------------------------------
