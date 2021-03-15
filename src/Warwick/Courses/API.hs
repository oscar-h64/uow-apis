-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Courses.API ( 
    CoursesApi,
    getModule,
    searchModules 
) where 

-------------------------------------------------------------------------------

import Data.Text
import Data.Proxy

import Servant.API
import Servant.Client

import Warwick.Courses.Module
import Warwick.Courses.ModuleResult

-------------------------------------------------------------------------------

type CoursesApi
    = "modules" :>
      Capture "academicYear" Text :>
      Capture "moduleCode" Text :>
      Get '[JSON] Module
 :<|> "modules" :>
      QueryParam "keywords" Text :>
      QueryParam "departments" Text :>
      QueryParam "levels" Text :>
      QueryParam "creditValues" Text :>
      QueryParam "assessmentTypes" Text :>
      QueryParam "academicYears" Text :>
      QueryParam "page" Int :>
      Get '[JSON] [ModuleResult]

-- | A proxy value for the `CoursesApi` type.
courses :: Proxy CoursesApi
courses = Proxy

-------------------------------------------------------------------------------

getModule :: Text -> Text -> ClientM Module

searchModules 
    :: Maybe Text -- ^ Keywords
    -> Maybe Text -- ^ Departments
    -> Maybe Text -- ^ Levels
    -> Maybe Text -- ^ Credit values
    -> Maybe Text -- ^ Assessment types
    -> Maybe Text -- ^ Academic years
    -> Maybe Int 
    -> ClientM [ModuleResult]

getModule :<|> searchModules = client courses

-------------------------------------------------------------------------------
