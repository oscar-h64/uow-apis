--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Warwick.Common where 

--------------------------------------------------------------------------------

import Control.Monad.Except
import Control.Monad.State

import Data.Aeson
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.ISO8601

import Network.HTTP.Conduit

import Servant.API.BasicAuth
import Servant.Client

import Warwick.Config

--------------------------------------------------------------------------------

newtype DateTime = DateTime { getDateTime :: UTCTime }
    deriving (Eq, Show)

instance FromJSON DateTime where
    parseJSON = withText "ISO8601 date+time format" $
        \str -> case parseISO8601 (T.unpack str) of
            Nothing -> fail "Date and time not formatted in ISO8601."
            Just d  -> pure (DateTime d)

instance ToJSON DateTime where
    toJSON (DateTime time) = String (T.pack $ formatISO8601 time)

newtype Date = Date { getDate :: UTCTime }
    deriving (Eq, Show)

-- TODO: maybe this should only format the date component?
instance ToJSON Date where 
    toJSON (Date time) = String (T.pack $ formatISO8601 time)

instance FromJSON Date where
    parseJSON = withText "ISO8601 date format" $
        \str -> case parseTimeM True defaultTimeLocale "%F" (T.unpack str) of
            Nothing -> fail "Date not formatted in ISO8601."
            Just d  -> pure (Date d)

--------------------------------------------------------------------------------

class HasBaseUrl a where 
    getBaseUrl :: a -> BaseUrl

--------------------------------------------------------------------------------

data APIError
#if MIN_VERSION_servant_client(0,16,0)
    = TransportError ClientError
#else 
    = TransportError ServantError
#endif
    deriving Show

-- | Represents computations involving a Warwick API.
type Warwick = StateT APISession (ExceptT APIError ClientM)

-- | Represents the configuration for an API session.
data APISession = APISession {
    sessionAuthData :: BasicAuthData,
    sessionManager  :: Manager,
    sessionURL      :: BaseUrl
}

class Monad m => HasApiSession m where 
    getAuthData :: m BasicAuthData
    getManager :: m Manager
    getURL :: m BaseUrl

instance Monad m => HasApiSession (StateT APISession m) where 
    getAuthData = gets sessionAuthData

    getManager = gets sessionManager

    getURL = gets sessionURL

--------------------------------------------------------------------------------

withAPI ::
    HasBaseUrl i => i -> APIConfig -> Warwick a -> IO (Either APIError a)
withAPI inst APIConfig{..} m = do
    manager <- newManager tlsManagerSettings

    let auth = BasicAuthData
                    (encodeUtf8 apiUsername)
                    (encodeUtf8 apiPassword)
        url  = getBaseUrl inst
        env  = ClientEnv manager url
        sesh = APISession auth manager url

    r <- runClientM (runExceptT $ evalStateT m sesh) (env Nothing)

    case r of
        Left serr -> pure $ Left $ TransportError serr
        Right res -> pure res

--------------------------------------------------------------------------------
