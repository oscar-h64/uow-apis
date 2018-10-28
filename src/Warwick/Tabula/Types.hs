--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Types (
    module UUID,

    module Warwick.Tabula.Date,
    module Warwick.Tabula.JSON,

    HasPayload(..),

    ObjectList(..),
    AcademicYear(..),
    ModuleCode(..),
    AssignmentID(..),
    SubmissionID(..)
) where

--------------------------------------------------------------------------------

import Data.String
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Proxy
import Data.UUID.Types as UUID

import Servant.API

import Warwick.Tabula.Date
import Warwick.Tabula.JSON

--------------------------------------------------------------------------------

class HasPayload a where
    payloadFieldName :: Proxy a -> T.Text

newtype ObjectList a = ObjectList { getList :: [a] }

instance FromJSON a => FromJSON (ObjectList a) where
    parseJSON = withObject "Tabula object array" $ \obj ->
        ObjectList <$> mapM (parseJSON . snd) (HM.toList obj)

--------------------------------------------------------------------------------

instance IsString UUID where
    fromString str = case UUID.fromString str of
        Nothing   -> error "IsString UUID: Not a valid UUID."
        Just uuid -> uuid

type AcademicYear = String

newtype ModuleCode = ModuleCode { moduleCode :: BS.ByteString }
    deriving IsString

instance ToHttpApiData ModuleCode where
    toQueryParam (ModuleCode mc) = T.decodeUtf8 mc

newtype AssignmentID = AssignmentID { unAssignmentID :: UUID }
    deriving (IsString)

instance Show AssignmentID where
    show (AssignmentID uuid) = show uuid

instance FromJSON AssignmentID where
    parseJSON v = AssignmentID <$> parseJSON v

newtype SubmissionID = SubmissionID { unSubmissionID :: UUID }
    deriving (IsString)

--------------------------------------------------------------------------------