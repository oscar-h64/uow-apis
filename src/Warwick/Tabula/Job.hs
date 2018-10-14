--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Job where

--------------------------------------------------------------------------------

import GHC.Generics

import Data.Aeson

import Warwick.Tabula.Types

--------------------------------------------------------------------------------

data JobInstance = JobInstance {
    jobID :: UUID,
    jobType :: String,
    jobProgress :: Int,
    jobStarted :: Bool,
    jobFinished :: Bool,
    jobSuccessful :: Bool,
    jobStatus :: String,
    jobUser :: String,
    jobCreated :: TabulaDateTime,
    jobUpdated :: TabulaDateTime,
    jobData :: String
} deriving (Show, Generic)

instance FromJSON JobInstance where
    parseJSON = parseTabulaJSON

instance HasPayload JobInstance where
    payloadFieldName _ = "job"

--------------------------------------------------------------------------------
