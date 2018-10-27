module Warwick.DownloadSubmission where

-------------------------------------------------------------------------------

import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (httpLbs)
import qualified Network.HTTP.Client.Conduit as C

import Servant.API.BasicAuth
import Servant.Client hiding (responseBody)

import Warwick.Tabula.TabulaSession
import Warwick.Tabula.Types
import Warwick.Tabula.Coursework

-------------------------------------------------------------------------------

buildDownloadURL :: BaseUrl
                 -> ModuleCode
                 -> AssignmentID
                 -> Submission
                 -> FilePath
                 -> String
buildDownloadURL url mc aid sub fn = concat
    [ baseUrlPath url
    , "/module/"
    , C8.unpack (moduleCode mc)
    , "/assignments/"
    , C8.unpack (toASCIIBytes $ unAssignmentID aid)
    , "/submissions/"
    , (submissionID sub)
    , "/"
    , fn
    ]

downloadSubmission :: String
                   -> ModuleCode
                   -> AssignmentID
                   -> Submission
                   -> FilePath
                   -> FilePath
                   -> Tabula ()
downloadSubmission sid mc aid sub fn out = do
    manager            <- tabulaManager
    baseURL            <- tabulaURL
    BasicAuthData {..} <- tabulaAuthData
    req <- parseRequest ("https://" ++ baseUrlHost baseURL ++ buildDownloadURL baseURL mc aid sub fn)
    let
        request
            = applyBasicAuth basicAuthUsername basicAuthPassword
            $ setRequestMethod "GET"
            -- $ setRequestPath url
            $ setRequestSecure True
            $ setRequestPort (baseUrlPort baseURL)
            $ setRequestHost (BS.packChars $ baseUrlHost baseURL)
            $ setRequestCheckStatus
            $ req
    response <- liftIO $ httpLbs request manager
    liftIO $ LBS.writeFile out (responseBody response)
    return ()
