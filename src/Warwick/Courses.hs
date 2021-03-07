-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Courses (
    CoursesInstance(..),
    API.getModule
) where

-------------------------------------------------------------------------------

import Control.Monad.Trans

import Data.Aeson

import Servant.Client

import Warwick.Common
import Warwick.Config
import qualified Warwick.Courses.API as API

-------------------------------------------------------------------------------

-- | Enumerates Course Catalogue API instances.
data CoursesInstance = Live | Dev | CustomInstance BaseUrl
    deriving (Eq, Show)

instance ToJSON CoursesInstance where 
    toJSON Live = String "live"
    toJSON Dev  = String "dev"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON CoursesInstance where 
    parseJSON (String "live") = pure Live
    parseJSON (String "dev")  = pure Dev
    parseJSON val = flip (withObject "CoursesInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | The URL to the live Course Catalogue instance.
liveURL :: BaseUrl
liveURL = BaseUrl Https "courses.warwick.ac.uk" 443 ""

-- | The URL to the dev Course Catalogue instance.
devURL :: BaseUrl
devURL = BaseUrl Https "courses-dev.warwick.ac.uk" 443 ""

instance HasBaseUrl CoursesInstance where
    getBaseUrl Live                 = liveURL
    getBaseUrl Dev                  = devURL
    getBaseUrl (CustomInstance url) = url

-------------------------------------------------------------------------------
