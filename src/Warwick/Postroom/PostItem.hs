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

data Courier = Courier {
    cId :: Maybe UUID,
    cCreatedAt :: Maybe UTCTime,
    cName :: Text,
    cShortCode :: Text
}

instance FromJSON Courier where
    parseJSON = withObject "Courier" $ \v ->
        Courier <$> v .: "id"
                <*> v .: "createdAt"
                <*> v .: "name"
                <*> v .: "shortCode"

-------------------------------------------------------------------------------

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

data PostItem = PostItem {
    piId :: Maybe UUID,
    piBarcode :: Maybe Text,
    piCollectionBy :: Maybe UTCTime,
    piCourier :: Maybe Courier,
    piCreatedAt :: Maybe UTCTime,
    piLocationReference :: Maybe Text,
    piNomineeUniversityId :: Maybe Text,
    piRecipient :: Maybe Recipient,
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
