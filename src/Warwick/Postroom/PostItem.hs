-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Postroom.PostItem (
    Courier(..),
    PostItemStatus(..),
    PostItem(..)
) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text (unpack,  Text )
import Data.Time ( UTCTime )
import Data.UUID ( UUID )

import Warwick.Postroom.Recipient ( Recipient )

-------------------------------------------------------------------------------

-- | Represents a courier in the Warwick system
data Courier = Courier {
    -- | The unique ID of the courier used by the university if included
    cId :: Maybe UUID,
    -- | The time the courier was added to the database if included
    cCreatedAt :: Maybe UTCTime,
    -- | The name of the courier
    cName :: Text,
    -- | A short code representing the courier
    cShortCode :: Text
}

instance FromJSON Courier where
    parseJSON = withObject "Courier" $ \v ->
        Courier <$> v .: "id"
                <*> v .: "createdAt"
                <*> v .: "name"
                <*> v .: "shortCode"

-------------------------------------------------------------------------------

-- | Represents the possible statuses of postroom items
data PostItemStatus = DeliveredNowOnShelf
                    | ForwardedToAccommodation
                    | PickedUpByStudent
                    | ReturnToSenderAwaitingPickup
                    | ReturnToSenderGone

instance FromJSON PostItemStatus where
    parseJSON = withText "PostItemStatus" $ \t -> case t of
        "DeliveredNowOnShelf" -> pure DeliveredNowOnShelf
        "ForwardedToAccommodation" -> pure ForwardedToAccommodation
        "PickedUpByStudent" -> pure PickedUpByStudent
        "ReturnToSenderAwaitingPickup" -> pure ReturnToSenderAwaitingPickup
        "ReturnToSenderGone" -> pure ReturnToSenderGone
        x -> fail $ "Invalid Post Item Status: " ++ unpack x

-------------------------------------------------------------------------------

-- | Represents a post item in the Warwick post system
data PostItem = PostItem {
    -- | The unique ID of the item if included
    piId :: Maybe UUID,
    -- | The barcode of the item in the postroom if included
    piBarcode :: Maybe Text,
    -- | The deadline to collect the item by if included
    piCollectionBy :: Maybe UTCTime,
    -- | The courier that delivered the item if included
    piCourier :: Maybe Courier,
    -- | The time the item was added to the system if included
    piCreatedAt :: Maybe UTCTime,
    -- | Where the parcel is currently located if included
    piLocationReference :: Maybe Text,
    -- | The university ID of the non-recipient authorised to collect the item
    -- if there is one
    piNomineeUniversityId :: Maybe Text,
    -- | The recipient of the item if included
    piRecipient :: Maybe Recipient,
    -- | The current status of the item
    piStatus :: PostItemStatus
}

instance FromJSON PostItem where
    parseJSON = withObject "OpeningTimes" $ \v ->
        PostItem <$> v .: "id"
                 <*> v .: "barcode"
                 <*> v .: "collectionBy"
                 <*> v .: "courier"
                 <*> v .: "createdAt"
                 <*> v .: "locationReference"
                 <*> v .: "nomineeUniversityId"
                 <*> v .: "recipient"
                 <*> v .: "status"

-------------------------------------------------------------------------------
