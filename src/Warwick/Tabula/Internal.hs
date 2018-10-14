--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Internal where

--------------------------------------------------------------------------------

import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString as BS

import Servant.API
import Servant.Client

import Warwick.Tabula.Types
import Warwick.Tabula.API
import Warwick.Tabula.Attachment
import Warwick.Tabula.Coursework
import Warwick.Tabula.Job

--------------------------------------------------------------------------------

listAssignments :: BasicAuthData -> ModuleCode -> Maybe AcademicYear -> ClientM (TabulaResponse [Assignment])
listSubmissions :: BasicAuthData -> ModuleCode -> UUID -> ClientM (TabulaResponse (HM.HashMap String (Maybe Submission)))

uploadAttachment :: BasicAuthData -> Maybe String -> BS.ByteString -> ClientM (TabulaResponse Attachment)

retrieveJob :: BasicAuthData -> UUID -> ClientM (TabulaResponse JobInstance)

-- | Automatically generate client functions.
listAssignments :<|>
    listSubmissions :<|>
    uploadAttachment :<|>
    retrieveJob = client tabula

--------------------------------------------------------------------------------
