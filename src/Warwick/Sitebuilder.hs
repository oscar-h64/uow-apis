--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Warwick.Sitebuilder (
    module Warwick.Config,
    module Warwick.Common,
    module API,

    SitebuilderInstance(..),

    editPage,
    editPageFromFile,
    pageInfo,
    uploadFile
) where 

--------------------------------------------------------------------------------

import Control.Monad.Except
import Control.Monad.Reader

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Text
import Data.Text.Encoding


import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (httpLbs, withResponse)
import Network.Mime

import Servant.API.BasicAuth
import Servant.Client

import Warwick.Config
import Warwick.Common

import qualified Warwick.Sitebuilder.API as API
import qualified Warwick.Sitebuilder.PageInfo as API
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

pageInfo :: Text -> Warwick API.PageInfo
pageInfo page = do 
    authData <- getAuthData
    lift $ lift $ API.pageInfo authData (Just page)

uploadFile :: Text -> Text -> FilePath -> Warwick ()
uploadFile page slug fp = do 
    manager            <- getManager
    baseURL            <- getURL
    BasicAuthData {..} <- getAuthData
    req <- parseRequest ("https://" ++ baseUrlHost baseURL ++ baseUrlPath baseURL ++ "/edit/atom/file.htm?page=" ++ unpack page)
    let
        mimeType = defaultMimeLookup (pack fp) 
        request
            = applyBasicAuth basicAuthUsername basicAuthPassword
            $ setRequestMethod "POST"
            $ setRequestSecure True
            $ setRequestPort (baseUrlPort baseURL)
            $ setRequestHost (BS.packChars $ baseUrlHost baseURL)
            $ setRequestCheckStatus
            $ setRequestHeader "Slug" [encodeUtf8 slug]
            $ setRequestHeader "Content-Type" [mimeType]
            $ setRequestBodyFile fp
            $ req
    response <- liftIO $ httpLbs request manager
    pure ()

--------------------------------------------------------------------------------