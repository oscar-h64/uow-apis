--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Attachment where

--------------------------------------------------------------------------------

import GHC.Generics

import Data.Aeson

import Warwick.Tabula.Types

--------------------------------------------------------------------------------

data Attachment = Attachment {
    attachmentID        :: UUID,
    attachmentTemporary :: Maybe Bool,
    attachmentHash      :: Maybe String,
    attachmentFilename  :: String--,
    --attachmentUploadedBy :: String,
    --attachmentDateUploaded :: TabulaDateTime
} deriving (Eq, Show, Generic)

instance FromJSON Attachment where
    parseJSON = parseTabulaJSON

instance HasPayload Attachment where
    payloadFieldName _ = "attachment"

--------------------------------------------------------------------------------
