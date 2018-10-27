module Warwick.Tabula.TabulaSession where

-------------------------------------------------------------------------------

import Control.Monad.State
import Control.Monad.Except

import Network.HTTP.Conduit

import Servant.API.BasicAuth
import Servant.Client

import Warwick.Tabula.Error

-------------------------------------------------------------------------------

-- | Represents computations involving the Tabula API.
type Tabula = StateT TabulaSession (ExceptT TabulaError ClientM)

-- | Represents the configuration for a Tabula session.
data TabulaSession = TabulaSession {
    sessionAuthData :: BasicAuthData,
    sessionManager  :: Manager,
    sessionURL      :: BaseUrl
}

tabulaAuthData :: Tabula BasicAuthData
tabulaAuthData = gets sessionAuthData

tabulaManager :: Tabula Manager
tabulaManager = gets sessionManager

tabulaURL :: Tabula BaseUrl
tabulaURL = gets sessionURL
