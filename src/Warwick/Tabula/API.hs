--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.API where

--------------------------------------------------------------------------------

import Data.Aeson
import qualified Data.HashMap.Lazy as HM
import Data.ByteString as BS
import Data.UUID.Types
import Data.Proxy

import Servant.API

import Warwick.Tabula.Types
import Warwick.Tabula.Error
import Warwick.Tabula.Attachment
import Warwick.Tabula.Coursework
import Warwick.Tabula.Job

--------------------------------------------------------------------------------

-- | Represents Tabula responses with payloads of type a.
data TabulaResponse a
    = TabulaOK {
        tabulaStatus  :: String,
        tabulaData    :: a
    }
    {- | TabulaError {
        tabulaStatus  :: String,
        tabulaError   :: TabulaError
    }  -} deriving (Show)

instance (HasPayload a, FromJSON a) => FromJSON (TabulaResponse a) where
    parseJSON = withObject "TabulaResponse" $ \v -> do
        s <- v .: "success"
        if s
        then TabulaOK <$> v .: "status"
                      <*> v .: payloadFieldName (Proxy :: Proxy a)
        else fail "200 OK, but success is not true (this should not happen)"

type TabulaAuth = BasicAuth "" ()

type Coursework =
      TabulaAuth :> "module" :> Capture "moduleCode" ModuleCode :> "assignments" :> QueryParam "academicYear" String :> Get '[JSON] (TabulaResponse [Assignment])
 :<|> TabulaAuth :> "module" :> Capture "moduleCode" ModuleCode :> "assignments" :> Capture "assignmentId" UUID :> "submissions" :> Get '[JSON] (TabulaResponse (HM.HashMap String (Maybe Submission)))
 --  :<|> TabulaAuth :> "module" :> Capture "moduleCode" String :> "assignments" :> Capture "assignmentId" String :> "submissions" :> Capture "submissionId" String :> Capture "filename" String :> Raw
 :<|> TabulaAuth :> "attachments" :> QueryParam "filename" String :> ReqBody '[OctetStream] BS.ByteString :> Post '[JSON] (TabulaResponse Attachment)
 :<|> TabulaAuth :> "job" :> Capture "jobID" UUID :> Get '[JSON] (TabulaResponse JobInstance)
type API = Coursework

tabula :: Proxy API
tabula = Proxy

--------------------------------------------------------------------------------
