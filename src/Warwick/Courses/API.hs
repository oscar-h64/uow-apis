-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Courses.API ( 
    CoursesApi,
    getModule 
) where 

-------------------------------------------------------------------------------

import Data.Text
import Data.Proxy

import Servant.API
import Servant.Client

import Warwick.Courses.Module

-------------------------------------------------------------------------------

type CoursesApi
    = "modules" :>
      Capture "academicYear" Text :>
      Capture "moduleCode" Text :>
      Get '[JSON] Module

-- | A proxy value for the `CoursesApi` type.
courses :: Proxy CoursesApi
courses = Proxy

-------------------------------------------------------------------------------

getModule :: Text -> Text -> ClientM Module

getModule = client courses

-------------------------------------------------------------------------------
