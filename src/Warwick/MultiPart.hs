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

import Data.ByteString
import qualified Data.HashMap.Lazy as HM
import Data.Text

import Network.HTTP.Media ((//), (/:))

import Servant.API

-------------------------------------------------------------------------------

data MultiPart

boundary :: ByteString
boundary = "------------------------------"

instance Accept MultiPart where
    contentType _ = "multipart" // "form-data" /: ("boundary", boundary)

-------------------------------------------------------------------------------

type MultiPartForm = HM.HashMap Text Part

data Part = Field { fieldValue :: Text }
          | File { fileName :: Text
                 , fileType :: Text
                 , fileContents :: ByteString
                 }

-------------------------------------------------------------------------------

class ToMultiPartForm a where
    toMultiPartForm :: a -> MultiPartForm

instance ToMultiPartForm MultiPartForm where
    toMultiPartForm = id

-------------------------------------------------------------------------------

instance ToMultiPartForm a => MimeRender MultiPart a where
    mimeRender = undefined

-------------------------------------------------------------------------------
