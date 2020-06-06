-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.MyWarwick.Response (
    MyWarwickError(..),
    ppMyWarwickError,
    StreamID(..),
    MyWarwickResponse(..)
) where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text as T
import Data.UUID.Types

--------------------------------------------------------------------------------

-- | Represents errors returned by the MyWarwick API.
data MyWarwickError = MyWarwickError {
    -- | The ID (e.g. @no-permission@) of the error.
    mweID :: Text, 
    -- | The message of the error.
    mweMessage :: Text
} deriving (Eq, Show)

-- | 'ppMyWarwickError' @error@ pretty-prints @error@.
ppMyWarwickError :: MyWarwickError -> Text 
ppMyWarwickError MyWarwickError{..} = T.concat 
    [ "[", mweID, "] ", mweMessage ]

instance ToJSON MyWarwickError where 
    toJSON MyWarwickError{..} =
        object [ "id" .= mweID
               , "message" .= mweMessage
               ]

instance FromJSON MyWarwickError where 
    parseJSON = withObject "MyWarwickError" $ \obj ->
        MyWarwickError <$> obj .: "id"
                       <*> obj .: "message"

-- | Represents responses to requests that create stream items.
data StreamID = StreamID {
    -- | The UUID of the stream item that was created.
    streamID :: UUID
} deriving (Eq, Show)

instance ToJSON StreamID where 
    toJSON (StreamID val) = object [ "id" .= val ]

instance FromJSON StreamID where 
    parseJSON = withObject "StreamID" $ \obj ->
        StreamID <$> obj .: "id"

-- | Represents responses from the MyWarwick API.
data MyWarwickResponse = MyWarwickResponse {
    -- | A value indicating whether the request was successful.
    mwrSuccess :: Bool,
    -- | A status message.
    mwrStatus :: Text,
    -- | If the request was successful, this contains any data that
    -- was returned.
    mwrData :: Maybe StreamID,
    -- | If the request failed, this contains a list of errors.
    mwrErrors :: Maybe [MyWarwickError]
} deriving (Eq, Show)

instance ToJSON MyWarwickResponse where 
    toJSON MyWarwickResponse{..} =
        object [ "success" .= mwrSuccess
               , "status" .= mwrStatus
               , "data" .= mwrData
               , "errors" .= mwrErrors
               ]

instance FromJSON MyWarwickResponse where
    parseJSON = withObject "MyWarwickResponse" $ \obj ->
        MyWarwickResponse <$> obj .: "success"
                          <*> obj .: "status"
                          <*> obj .:? "data"
                          <*> obj .:? "errors"

--------------------------------------------------------------------------------
