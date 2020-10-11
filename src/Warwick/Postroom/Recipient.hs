-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Postroom.Recipient (
    PostroomHub(..),
    AccommodationBlock(..),
    RecipientSource(..),
    RecipientType(..),
    Recipient(..)
) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.UUID ( UUID )

-------------------------------------------------------------------------------

-- | Represents a Warwick postroom hub
data PostroomHub = PostroomHub {
    -- | The unique ID of the hub
    phId :: UUID,
    -- | The name of the hub
    phName :: Text,
    -- | Where the hub is on campus
    phLocation :: Text,
    -- | The ID of the location on the campus map
    phMapId :: Text
}

instance FromJSON PostroomHub where
    parseJSON = withObject "PostroomHub" $ \v ->
        PostroomHub <$> v .: "id"
                    <*> v .: "name"
                    <*> v .: "location"
                    <*> v .: "mapId"

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

-- TODO: Some of these should probably be Maybe
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
    rValidFrom :: UTCTime,
    rValidTo :: UTCTime
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
