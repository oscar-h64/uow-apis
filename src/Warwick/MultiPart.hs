-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.MultiPart (
    MultiPart,

    MultiPartForm,
    Part(..),

    ToMultiPartForm(..)
) where

-------------------------------------------------------------------------------

import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.HashMap.Lazy as HM

import Network.HTTP.Media ((//), (/:))

import Servant.API

-------------------------------------------------------------------------------

data MultiPart

boundary :: ByteString
boundary = "------------------------------"

instance Accept MultiPart where
    contentType _ = "multipart" // "form-data"
                                /: ("boundary", toStrict boundary)

-------------------------------------------------------------------------------

type MultiPartForm = HM.HashMap ByteString Part

data Part = Field { fieldValue :: ByteString }
          | File { fileName :: ByteString
                 , fileType :: ByteString
                 , fileContents :: ByteString
                 }

-------------------------------------------------------------------------------

class ToMultiPartForm a where
    toMultiPartForm :: a -> MultiPartForm

instance ToMultiPartForm MultiPartForm where
    toMultiPartForm = id

-------------------------------------------------------------------------------

instance ToMultiPartForm a => MimeRender MultiPart a where
    mimeRender _ object =
        let form = toMultiPartForm object

            renderPart (name, Field{..}) = mconcat
                [ "--", boundary, "\r\n"
                , "Content-Disposition: form-data; name=\"", name, "\"\r\n"
                , "\r\n"
                , fieldValue, "\r\n"
                ]
            renderPart (name, File{..}) = mconcat
                [ "--", boundary, "\r\n"
                , "Content-Disposition: form-data; name=\"", name, "\"; ",
                    "filename=\"", fileName, "\"\r\n"
                , "Content-Type: ", fileType, "\r\n"
                , "\r\n"
                , fileContents, "\r\n"
                ]

            endBoundary = "--" <> boundary <> "--\r\n"

        in mconcat (map renderPart $ HM.toList form) <> endBoundary

-------------------------------------------------------------------------------
