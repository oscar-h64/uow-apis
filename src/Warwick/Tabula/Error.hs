--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Error where

--------------------------------------------------------------------------------

import Servant.Client (ServantError)

--------------------------------------------------------------------------------

data TabulaError
    = BadRequest
    | Unauthorized
    | RequestFailed
    | Forbidden
    | NotFound
    | ServerError
    | TransportError ServantError
    deriving Show

--------------------------------------------------------------------------------
