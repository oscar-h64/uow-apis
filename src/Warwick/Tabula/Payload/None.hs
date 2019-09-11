--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.None where

--------------------------------------------------------------------------------

import Data.Aeson

import Warwick.Tabula.Types

--------------------------------------------------------------------------------

data None = None deriving (Eq, Show)

instance FromJSON None where 
    parseJSON _ = pure None

instance HasPayload None where 
    payloadFieldName _ = error "No payload for None"
    payload _ = pure None

--------------------------------------------------------------------------------
