-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.AEP.FileUpload (
    FileUpload(..)
) where

-------------------------------------------------------------------------------

import Data.ByteString.Lazy
import qualified Data.HashMap.Lazy as HM
import Data.Text
import Data.Text.Encoding

import Servant.API

import Warwick.MultiPart

-------------------------------------------------------------------------------

data FileUpload = MkFileUpload {
    fuFileName :: Text,
    fuFileType :: ByteString,
    fuFileContents :: ByteString,
    fuOverwrite :: Bool
}

instance ToMultiPartForm FileUpload where
    toMultiPartForm MkFileUpload{..} =
        HM.fromList [ ("overwrite", Field $ fromStrict $ toHeader fuOverwrite)
                    , ("xhr", Field "true")
                    , ("file", File (fromStrict $ encodeUtf8 fuFileName)
                                    fuFileType
                                    fuFileContents
                      )
                    ]

-------------------------------------------------------------------------------
