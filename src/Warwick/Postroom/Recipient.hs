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
import Data.UUID.Types ( UUID )

import Warwick.Postroom.PostroomHub ( PostroomHub )

-------------------------------------------------------------------------------

-- | Represents an accommodation block
data AccommodationBlock = AccommodationBlock {
    -- | The unique ID of the block
    abId :: UUID,
    -- | The abriviation for the block, for example JM2, if it can be extracted
    -- from the room
    abCode :: Maybe Text,
    -- | The name of the block, for example "Jack Martin 2"
    abName :: Text,
    -- | The postroom hub used for this block if included
    abHub :: Maybe PostroomHub,
    -- | The ID for the block in kinetic
    abKineticId :: Int,
    -- | The ID of the blocks parent in kinetic if the block has a parent
    abKineticParentId :: Maybe Int,
    -- | The ID for the site in kinetic the accommodation block is located in
    abKineticSiteId :: Int
} deriving (Eq, Show)

instance FromJSON AccommodationBlock where
    parseJSON = withObject "AccommodationBlock" $ \v ->
        AccommodationBlock <$> v .: "id"
                           <*> v .:? "code"
                           <*> v .: "name"
                           <*> v .:? "hub"
                           <*> v .: "kineticId"
                           <*> v .:? "kineticParentId"
                           <*> v .: "kineticSiteId"

-------------------------------------------------------------------------------

-- | Represents the source the recipient has been added from
data RecipientSource = ConferenceImport | KineticImport | ManualEntry
    deriving (Eq, Show)

instance FromJSON RecipientSource where
    parseJSON = withText "RecipientSource" $ \t -> case t of
        "ConferenceImport" -> pure ConferenceImport
        "KineticImport" -> pure KineticImport
        "ManualEntry" -> pure ManualEntry
        x -> fail $ "Invalid Recipient Source: " ++ unpack x

-------------------------------------------------------------------------------

-- | Represents the type of the resident
data RecipientType = CentreForAppliedLinguistics
                   | MathematicsResearchCentre
                   | ResidentialLifeTeam
                   | StaffFamily
                   | Student
                   | VacationTenant       
    deriving (Eq, Show)

instance FromJSON RecipientType where
    parseJSON = withText "RecipientType" $ \t -> case t of
        "CAL" -> pure CentreForAppliedLinguistics
        "MRC" -> pure MathematicsResearchCentre
        "RLT" -> pure ResidentialLifeTeam
        "StaffFamily" -> pure StaffFamily
        "Student" -> pure Student
        "VacationTenant" -> pure VacationTenant
        x -> fail $ "Invalid Recipient Type: " ++ unpack x

-------------------------------------------------------------------------------

-- | Represents an addressee at Warwick
data Recipient = Recipient {
    -- | The unique ID of the recipient
    rId :: UUID,
    -- | The accommodation the recipient lives in such as Jack Martin if
    -- the room comes from kinetic
    rAccommodationBlock :: Maybe AccommodationBlock,
    -- | The recipients first name
    rFirstName :: Text,
    -- | The recipients middle name if there is one stored
    rMiddleName :: Maybe Text,
    -- | The recipients last name
    rLastName :: Text,
    -- | The recipients preferred name if there is one stored
    rPreferredName :: Maybe Text,
    -- | Whether the recipient is currently active
    rInactive :: Bool,
    -- | The floor number of the room if the recipient is a kinetic import
    rKineticFloor :: Maybe Int,
    -- | The room number of the recipient as stored in kinetic, if they are
    -- a kinetic import
    rKineticRoom :: Maybe Text,
    -- | The room the recipient lives in if applicable
    rRoom :: Maybe Text,
    -- | Where the recipient has been imported from
    rSource :: RecipientSource,
    -- | The specific block the recipient lives in such as Jack Martin 2 if the
    -- recipient comes from kinetic
    rSubAccommodationBlock :: Maybe AccommodationBlock,
    -- | The type of the recipient, such as Student or RLT
    rType :: RecipientType,
    -- | The recipient's univeristy ID
    rUniversityId :: Text,
    -- | When the recipient is active from if the resident is not a kinetic
    -- import
    rValidFrom :: Maybe Day,
    -- | When the recipient is active to if the resident is not a kinetic
    -- import
    rValidTo :: Maybe Day,
    -- | The date the recipient is self-isolating to, if they are currently
    -- self-isolating
    rSelfIsolatingUntil :: Maybe Day
} deriving (Eq, Show)

instance FromJSON Recipient where
    parseJSON = withObject "Recipient" $ \v ->
        Recipient <$> v .: "id"
                  <*> v .:? "accommodationBlock"
                  <*> v .: "firstName"
                  <*> v .:? "middleName"
                  <*> v .: "lastName"
                  <*> v .:? "preferredName"
                  <*> v .: "inactive"
                  <*> v .:? "kineticFloor"
                  <*> v .:? "kineticRoom"
                  <*> v .:? "room"
                  <*> v .: "source"
                  <*> v .:? "subAccommodationBlock"
                  <*> v .: "type"
                  <*> v .: "universityId"
                  <*> v .:? "validFrom"
                  <*> v .:? "validTo"
                  <*> v .:? "selfIsolatingUntil"

-------------------------------------------------------------------------------
