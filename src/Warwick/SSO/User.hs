--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.SSO.User (
    UserClass(..),
    User(..)
) where 

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.HashMap.Lazy as HM
import Data.Text

import Servant.API

import Xeno.DOM

import Warwick.XML
import Warwick.SSO.YesNo

--------------------------------------------------------------------------------

-- | Enumerates different classes of users.
data UserClass
    = Staff
    | Student
    | Alumni
    | Applicant

instance ToHttpApiData UserClass where 
    toQueryParam Staff = "Staff"
    toQueryParam Student = "Student"
    toQueryParam Alumni = "Alumni"
    toQueryParam Applicant = "Applicant"

-- | Represents SSO user objects.
data User = User {
    -- | The university ID of the user.
    userWarwickUniID :: Text,
    -- | The username (common name) of the user.
    userUsername :: Text,
    -- | The user's email address.
    userMail :: Maybe Text,
    -- | The user's given name.
    userGivenName :: Text,
    -- | The user's surname.
    userSurname :: Text, 
    -- | The department code of the user's primary department.
    userWarwickDeptCode :: Text,
    -- | The short name of the user's primary department.
    userWarwickDeptShort :: Text,
    -- | The full name of the user's primary department.
    userDepartment :: Text,
    -- | The name of the user's organisational unit.
    userOrganisationalUnit :: Text, 
    -- | A value indicating whether this user is a student.
    userStudent :: Bool,
    -- | A value indicating whether this user is a member of staff.
    userStaff :: Bool,
    -- | A value indicating whether this user's account is disabled.
    userLoginDisabled :: Bool,
    -- | A value indicating whether this user's password is expired.
    userPasswordExpired :: Bool,
    -- | A value indicating whether this is the user's primary account.
    userWarwickPrimary :: Maybe YesNo
} deriving Show

-- | Represents a mapping of user attributes to their values.
type UserAttributes = HM.HashMap ByteString ByteString

-- | `elementsToHM` @elements@ converts a collection of XML elements into
-- a hashmap of user attributes.
elementsToHM :: [Node] -> Either String UserAttributes
elementsToHM [] = pure HM.empty
elementsToHM (n:ns) = do 
    hm <- elementsToHM ns 
    name <- getAttribute n "name"
    value <- getAttribute n "value"
    pure $ HM.insert name value hm

-- | `requireAttribute` @hashmap name@ tries to look up a user attribute
-- named @name@ in @hashmap@. If no entry for @name@ can be found, an
-- error is returned.
requireAttribute :: 
    FromByteString a => UserAttributes -> ByteString -> Either String a
requireAttribute hm name = case HM.lookup name hm of 
    Nothing  -> Left $ "No such user attribute: " ++ show name 
    Just val -> pure $ fromByteString val

-- | `maybeAttribute` @hashmap name@ tries to look up a user attribute
-- named @name@ in @hashmap@. The resulting value is returned if @name@ can
-- be found or `Nothing` if not.
maybeAttribute :: 
    FromByteString a => UserAttributes -> ByteString -> Either String (Maybe a)
maybeAttribute hm name = pure (fromByteString <$> HM.lookup name hm)

instance FromXML User where 
    parseXML node = do 
        -- construct a hashmap of user attributes
        hm <- elementsToHM (children node)

        User <$> requireAttribute hm "warwickuniid"
             <*> requireAttribute hm "cn"
             <*> maybeAttribute hm "mail"
             <*> requireAttribute hm "givenName"
             <*> requireAttribute hm "sn"
             <*> requireAttribute hm "warwickdeptcode"
             <*> requireAttribute hm "deptshort"
             <*> requireAttribute hm "department"
             <*> requireAttribute hm "ou"
             <*> requireAttribute hm "student"
             <*> requireAttribute hm "staff"
             <*> requireAttribute hm "logindisabled"
             <*> requireAttribute hm "passwordexpired"
             <*> maybeAttribute hm "warwickprimary"

--------------------------------------------------------------------------------