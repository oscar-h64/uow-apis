-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Relationship (
    RelationshipType(..),
    RelationshipEntry(..),
    Relationship(..),
    relationshipsOfType
) where

--------------------------------------------------------------------------------

import Data.Aeson

import Warwick.Tabula.Types

--------------------------------------------------------------------------------

-- | Represents a description of a relationship type.
data RelationshipType = RelationshipType {
    relationshipTypeID          :: String,
    relationshipTypeAgentRole   :: String,
    relationshipTypeStudentRole :: String
} deriving Show

instance FromJSON RelationshipType where
    parseJSON = withObject "relationship type" $ \v ->
        RelationshipType <$> v .: "id"
                         <*> v .: "agentRole"
                         <*> v .: "studentRole"

data RelationshipEntry = RelationshipEntry {
    relationshipEntryUserID       :: String,
    relationshipEntryUniversityID :: String
} deriving Show

instance FromJSON RelationshipEntry where
    parseJSON = withObject "relationship entry" $ \v ->
        RelationshipEntry <$> v .: "userId"
                          <*> v .: "universityId"

-- | Represents a relationship that a user has with a group of students.
data Relationship = Relationship {
    relationshipType     :: RelationshipType,
    relationshipStudents :: [RelationshipEntry]
} deriving Show

instance FromJSON Relationship where
    parseJSON = withObject "relationship" $ \v ->
        Relationship <$> v .: "relationshipType"
                     <*> v .: "students"

instance HasPayload [Relationship] where
    payloadFieldName _ = "relationships"

--------------------------------------------------------------------------------

-- | `relationshipsOfType` @rid retrieves all `Relationship` values where
-- the relationship type id is set to @rid.
relationshipsOfType :: String -> [Relationship] -> [Relationship]
relationshipsOfType rid = filter $ \Relationship{..} ->
    relationshipTypeID relationshipType == rid

--------------------------------------------------------------------------------
