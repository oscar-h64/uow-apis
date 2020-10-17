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

-- TODO: Check the example code and name aren't made up
-- | Represents an accommodation block
data AccommodationBlock = AccommodationBlock {
    -- | The unique ID of the block if included
    abId :: Maybe UUID,
    -- | The abriviation for the block, for example JM2 if included
    abCode :: Maybe Text,
    -- | The name of the block, for example "Jack Martin 2" if included
    abName :: Maybe Text,
    -- | The postroom hub used for this block if included
    abHub :: Maybe PostroomHub,
    -- | 
    abKineticId :: Maybe Int,
    -- | 
    abKineticParentId :: Maybe Int,
    -- | 
    abKineticSiteId :: Maybe Int
} deriving Show

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
    deriving Show

instance FromJSON RecipientSource where
    parseJSON = withText "RecipientSource" $ \t -> case t of
        "ConferenceImport" -> pure ConferenceImport
        "KineticImport" -> pure KineticImport
        "ManualEntry" -> pure ManualEntry
        x -> fail $ "Invalid Recipient Source: " ++ unpack x

-------------------------------------------------------------------------------

-- | Represents the type of the resident
data RecipientType = CAL | MRC | RLT | StaffFamily | Student | VacationTenant
    deriving Show

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

-- | Represents an addressee at Warwick
data Recipient = Recipient {
    -- | The unique ID of the recipient if included
    rId :: Maybe UUID,
    -- | The accommodation block the recipient lives in if applicable
    rAccommodationBlock :: Maybe AccommodationBlock,
    -- | The recipients first name
    rFirstName :: Text,
    -- | The recipients middle name if included
    rMiddleName :: Maybe Text,
    -- | The recipients last name
    rLastName :: Text,
    -- | The recipients preferred name if included
    rPreferredName :: Maybe Text,
    -- | Whether the recipient is currently active
    rInactive :: Bool,
    -- | 
    rKineticFloor :: Maybe Int,
    -- | 
    rKineticRoom :: Maybe Text,
    -- | The room the recipient lives in if applicable
    rRoom :: Maybe Text,
    -- | Where the recipient has been imported from
    rSource :: RecipientSource,
    -- |
    rSubAccommodationBlock :: Maybe AccommodationBlock,
    -- | The type of the recipient, such as Student or RLT
    rType :: RecipientType,
    -- | The recipient's univeristy ID
    rUniversityId :: Text,
    -- | When the recipient is active from if included
    rValidFrom :: Maybe Day,
    -- | When the recipient is active to if included
    rValidTo :: Maybe Day,
    -- | The date the recipient is self-isolating to, if they are currently
    -- self-isolating
    rSelfIsolatingUntil :: Maybe Day
} deriving Show

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
                  <*> v .: "selfIsolatingUntil"

-------------------------------------------------------------------------------
