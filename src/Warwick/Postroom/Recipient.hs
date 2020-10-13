-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Postroom.Recipient (
    AccommodationBlock(..),
    RecipientSource(..),
    RecipientType(..),
    Recipient(..)
) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text (unpack,  Text )
import Data.Time ( Day )
import Data.UUID ( UUID )

import Warwick.Postroom.PostroomHub ( PostroomHub )

-------------------------------------------------------------------------------

-- TODO: Check the example code and name aren't bullshit
-- | Represents an accommodation block
data AccommodationBlock = AccommodationBlock {
    -- | The unique ID of the block
    abId :: UUID,
    -- | The abriviation for the block, for example JM2
    abCode :: Text,
    -- | The name of the block, for example "Jack Martin 2"
    abName :: Text,
    -- | The postroom hub used for this block
    abHub :: PostroomHub,
    -- | 
    abKineticId :: Int,
    -- | 
    abKineticParentId :: Int,
    -- | 
    abKineticSiteId :: Int
}

instance FromJSON AccommodationBlock where
    parseJSON = withObject "AccommodationBlock" $ \v ->
        AccommodationBlock <$> v .: "id"
                           <*> v .: "code"
                           <*> v .: "hub"
                           <*> v .: "kineticId"
                           <*> v .: "kineticParentId"
                           <*> v .: "kineticSiteId"
                           <*> v .: "name"

-------------------------------------------------------------------------------

-- | Represents the source the recipient has been added from
data RecipientSource = ConferenceImport | KineticImport | ManualEntry

instance FromJSON RecipientSource where
    parseJSON = withText "RecipientSource" $ \t -> case t of
        "ConferenceImport" -> pure ConferenceImport
        "KineticImport" -> pure KineticImport
        "ManualEntry" -> pure ManualEntry
        x -> fail $ "Invalid Recipient Source: " ++ unpack x

-------------------------------------------------------------------------------

-- | Represents the type of the resident
data RecipientType = CAL | MRC | RLT | StaffFamily | Student | VacationTenant

instance FromJSON RecipientType where
    parseJSON = withText "RecipientType" $ \t -> case t of
        "CAL" -> pure CAL
        "MRC" -> pure MRC
        "RLT" -> pure RLT
        "StaffFamily" -> pure StaffFamily
        "Student" -> pure Student
        "VacationTenant" -> pure VacationTenant
        x -> fail $ "Invalid Recipient Type: " ++ unpack x

-------------------------------------------------------------------------------

-- TODO: Some of these should probably be Maybe
-- | Represents an addressee at Warwick
data Recipient = Recipient {
    rId :: UUID,
    rAccommodationBlock :: AccommodationBlock,
    rFirstName :: Text,
    rMiddleName :: Text,
    rLastName :: Text,
    rPreferredName :: Text,
    rInactive :: Bool,
    rKineticFloor :: Int,
    rKineticRoom :: Text,
    rRoom :: Text,
    rSource :: RecipientSource,
    rSubAccommodationBlock :: AccommodationBlock,
    rType :: RecipientType,
    rUniversityId :: Text,
    rValidFrom :: Day,
    rValidTo :: Day
}

instance FromJSON Recipient where
    parseJSON = withObject "Recipient" $ \v ->
        Recipient <$> v .: "id"
                  <*> v .: "accommodationBlock"
                  <*> v .: "firstName"
                  <*> v .: "middleName"
                  <*> v .: "lastName"
                  <*> v .: "preferredName"
                  <*> v .: "inactive"
                  <*> v .: "kineticFloor"
                  <*> v .: "kineticRoom"
                  <*> v .: "room"
                  <*> v .: "source"
                  <*> v .: "subAccommodationBlock"
                  <*> v .: "type"
                  <*> v .: "universityId"
                  <*> v .: "validFrom"
                  <*> v .: "validTo"

-------------------------------------------------------------------------------
