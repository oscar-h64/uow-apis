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

import Data.Text

import Servant.API
import Servant.Multipart

-------------------------------------------------------------------------------

data FileUpload = MkFileUpload {
    fuFilePath  :: FilePath,
    fuFileName  :: Text,
    fuFileType  :: Text,
    fuOverwrite :: Bool
}

instance ToMultipart Tmp FileUpload where
    toMultipart MkFileUpload{..} =
        MultipartData [ Input "overwrite" (toQueryParam fuOverwrite)
                      , Input "xhr" "true"
                      ]
                      [ FileData "file"
                                 fuFileName
                                 fuFileType
                                 fuFilePath

                      ]
        -- HM.fromList [ ("overwrite", Field $ fromStrict $ toHeader fuOverwrite)
        --             , ("xhr", Field "true")
        --             , ("file", File (fromStrict $ encodeUtf8 fuFileName)
        --                             fuFileType
        --                             fuFileContents
        --               )
        --             ]

-------------------------------------------------------------------------------
