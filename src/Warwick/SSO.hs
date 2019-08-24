--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.SSO (
    SSOInstance(..),

    SSO,
    withSSO,

    module Warwick.SSO.SearchParams,
    API.UserClass(..),
    userSearch
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

import qualified Warwick.SSO.API as API
import qualified Warwick.SSO.User as API
import qualified Warwick.SSO.YesNo as API
import Warwick.SSO.SearchParams

--------------------------------------------------------------------------------

-- | Enumerates SSO instances.
data SSOInstance = Live | Test | CustomInstance BaseUrl

instance ToJSON SSOInstance where 
    toJSON Live    = String "live"
    toJSON Test    = String "test"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON SSOInstance where 
    parseJSON (String "live")    = pure Live 
    parseJSON (String "test")    = pure Test
    parseJSON val = flip (withObject "SSOInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | The URL to the SSO API.
liveURL :: BaseUrl
liveURL = BaseUrl Https "websignon.warwick.ac.uk" 443 "/origin/api"

-- | The URL to the test instance of the SSO API.
testURL :: BaseUrl 
testURL = BaseUrl Https "websignon-test.warwick.ac.uk" 443 "/origin/api"

instance HasBaseUrl SSOInstance where
    getBaseUrl Live                 = liveURL
    getBaseUrl Test                 = testURL
    getBaseUrl (CustomInstance url) = url

-------------------------------------------------------------------------------

type SSO = ReaderT Text (ExceptT APIError ClientM)

-- | `withSSO` @instance apiKey computation@ performs a @computation@ 
-- which uses the Warwick SSO API by connecting to @instance@ using @apiKey@.
withSSO ::
    SSOInstance -> Text -> SSO a -> IO (Either APIError a)
withSSO inst apiKey m = do
    manager <- newManager tlsManagerSettings

    let url  = getBaseUrl inst
    let env  = ClientEnv manager url

    r <- runClientM (runExceptT (runReaderT m apiKey)) (env Nothing)

    case r of
        Left serr -> pure $ Left $ TransportError serr
        Right res -> pure res

-- | `searchProfiles` @apiKey params@ searches user profiles based on the
-- parameters specified in @params@.
userSearch ::
    SearchParams -> SSO [API.User]
userSearch SearchParams{..} = do 
    apiKey <- ask
    lift $ lift $ API.userSearch 
        (Just apiKey) 
        ssoSearchID 
        ssoSearchUsername 
        ssoSearchEmail 
        ssoSearchGivenName 
        ssoSearchSurname 
        ssoSearchDeptCode 
        ssoSearchDeptShort 
        ssoSearchDeptName 
        ssoSearchOrgUnit 
        ssoSearchUserClass
        ssoSearchLoginDisabled 
        ssoSearchPasswordExpired 
        (API.YesNo <$> ssoSearchPrimaryAccount)

--------------------------------------------------------------------------------