--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder (
    module Warwick.Config,
    module API,

    SitebuilderInstance(..),

    editPage,
    editPageFromFile
) where 

--------------------------------------------------------------------------------

import Control.Monad.Except
import Control.Monad.Reader

import Data.Aeson
import Data.Text

import Network.HTTP.Conduit

import Servant.Client

import Warwick.Config
import Warwick.Common

import qualified Warwick.Sitebuilder.API as API
import qualified Warwick.Sitebuilder.PageUpdate as API

--------------------------------------------------------------------------------

-- | Enumerates Sitebuilder instances.
data SitebuilderInstance = Live | CustomInstance BaseUrl

instance ToJSON SitebuilderInstance where 
    toJSON Live    = String "live"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON SitebuilderInstance where 
    parseJSON (String "live")    = pure Live 
    parseJSON val = flip (withObject "SitebuilderInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | The URL to the Sitebuilder API.
liveURL :: BaseUrl
liveURL = BaseUrl Https "sitebuilder.warwick.ac.uk" 443 "/sitebuilder2"

instance HasBaseUrl SitebuilderInstance where
    getBaseUrl Live                 = liveURL
    getBaseUrl (CustomInstance url) = url

-------------------------------------------------------------------------------

editPage :: Text -> API.PageUpdate -> Warwick ()
editPage page content = do 
    authData <- getAuthData
    lift $ lift $ API.editPage authData (Just page) (Just "single") content 

editPageFromFile :: Text -> Text -> FilePath -> Warwick ()
editPageFromFile page comment fp = do 
    contents <- liftIO $ readFile fp
    editPage page API.PageUpdate{
        puContents = pack contents,
        puChangeNote = comment
    }

--------------------------------------------------------------------------------