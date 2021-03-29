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
                [ boundary, "\n"
                , "Content-Disposition: form-data; name=\"", name, "\"\n"
                , "\n"
                , fieldValue, "\n"
                ]
            renderPart (name, File{..}) = mconcat
                [ boundary, "\n"
                , "Content-Disposition: form-data; name=\"", name, "\"; ",
                    "filename=\"", fileName, "\"\n"
                , "Content-Type: ", fileType, "\n"
                , "\n"
                , fileContents, "\n"
                ]

        in mconcat (map renderPart $ HM.toList form) <> boundary <> "--" 

-------------------------------------------------------------------------------
