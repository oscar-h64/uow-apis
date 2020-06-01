--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Types (
    module UUID,

    UUIDorString(..),

    module Warwick.Common,
    module Warwick.Tabula.JSON,

    Tabula,
    TabulaError(..),
    TabulaErr(..),

    HasPayload(..),

    ObjectList(..),
    AcademicYear,
    ModuleCode(..),
    AssignmentID(..),
    SubmissionID(..)
) where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.String
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Proxy
import Data.UUID.Types as UUID

import Servant.API
import Servant.Client

import Warwick.Common hiding (TransportError)
import Warwick.Tabula.JSON

--------------------------------------------------------------------------------

-- | Represents a Tabula error message.
data TabulaError = TabulaError (Maybe T.Text)
     deriving (Eq, Show)

instance FromJSON TabulaError where 
     parseJSON = withObject "TabulaError" $ \obj ->
          TabulaError <$> obj .: "message"

instance ToJSON TabulaError where 
    toJSON (TabulaError msg) = object [ "message" .= msg ]

-- | Represents computations involving the Tabula API.
type Tabula = StateT APISession (ExceptT TabulaErr ClientM)

data TabulaErr 
    = TransportError ClientError 
    | TabulaErrorRes {
         tabulaErrStatus   :: String,
         tabulaErrMessages :: [TabulaError]
    } 
    deriving (Eq, Show)

instance FromJSON TabulaErr where 
    parseJSON = withObject "TabulaErrorRes" $ \v ->
        TabulaErrorRes <$> v .: "status" <*> v .: "errors"

--------------------------------------------------------------------------------

class FromJSON a => HasPayload a where
    -- | `payloadFieldName` @proxy@ retrieves the payload filename.
    payloadFieldName :: Proxy a -> T.Text

    payload :: Object -> Parser a
    payload v = v .: payloadFieldName (Proxy :: Proxy a)

newtype ObjectList a = ObjectList { getList :: [a] }

instance FromJSON a => FromJSON (ObjectList a) where
    parseJSON = withObject "Tabula object array" $ \obj ->
        ObjectList <$> mapM (parseJSON . snd) (HM.toList obj)

--------------------------------------------------------------------------------

data UUIDorString = UUID UUID | NotUUID T.Text deriving Show

instance FromJSON UUIDorString where
    parseJSON (String v) = case fromText v of
        Nothing -> return $ NotUUID v
        Just uuid -> return $ UUID uuid
    parseJSON _ = mzero

instance IsString UUID where
    fromString str = case UUID.fromString str of
        Nothing   -> error "IsString UUID: Not a valid UUID."
        Just uuid -> uuid

type AcademicYear = String

newtype ModuleCode = ModuleCode { moduleCode :: T.Text }
    deriving IsString

instance ToHttpApiData ModuleCode where
    toQueryParam (ModuleCode mc) = mc

newtype AssignmentID = AssignmentID { unAssignmentID :: UUID }
    deriving (IsString)

instance Show AssignmentID where
    show (AssignmentID uuid) = show uuid

instance FromJSON AssignmentID where
    parseJSON v = AssignmentID <$> parseJSON v

newtype SubmissionID = SubmissionID { unSubmissionID :: UUID }
    deriving (IsString)

--------------------------------------------------------------------------------
