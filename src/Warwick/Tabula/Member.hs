-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Tabula.Member (
    MemberRelationship(..),
    Member(..),
    StudentCourseDetails(..),
    mostSignificantCourse
) where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text
import Data.Time
import qualified Data.Map as M

import Warwick.Tabula.Types
import Warwick.Tabula.Payload.CourseDetails
import Warwick.Tabula.Payload.Department

--------------------------------------------------------------------------------

data MemberField
    = MemberUniversityID
    | MemberFullName

instance Show MemberField where
    show MemberUniversityID = "member.universityId"
    show MemberFullName = "member.fullName"

data MemberRelationship = MemberRelationship {
    -- | The date and time when this relationship starts.
    mrStartDate :: ZonedTime,
    -- | The 'Member' who is the relationship agent.
    mrAgent :: Member
} deriving Show

instance Eq MemberRelationship where 
    l == r = mrAgent l == mrAgent r 
          && zonedTimeToUTC (mrStartDate l) == 
             zonedTimeToUTC (mrStartDate r)

instance FromJSON MemberRelationship where 
    parseJSON = withObject "MemberRelationship" $ \obj ->
        MemberRelationship <$> obj .: "startDate"
                           <*> obj .: "agent"

-- | Represents a member object.
data Member = Member {
    memberID            :: Maybe String,
    memberUserID        :: Maybe String,
    memberFirstName     :: Maybe String,
    memberFullFirstName :: Maybe String,
    memberLastName      :: Maybe String,
    memberEmail         :: Maybe String,
    memberUserType      :: Maybe String,
    memberFullName      :: Maybe String,
    memberOfficialName  :: Maybe String,
    memberHomeEmail     :: Maybe String,
    memberTitle         :: Maybe String,
    memberGender        :: Maybe String,
    memberInUse         :: Maybe String,
    memberJobTitle      :: Maybe String,
    memberPhoneNumber   :: Maybe String,
    memberGroupName        :: Maybe String,
    memberInactivationDate :: Maybe Date,
    memberStudentCourseDetails :: Maybe [StudentCourseDetails],
    -- | The member's home department.
    memberHomeDepartment :: Maybe DepartmentR,
    memberTouchedDepartments :: [DepartmentR],
    memberAffiliatedDepartments :: [DepartmentR]
} deriving (Eq, Show)

instance FromJSON Member where
    parseJSON = withObject "member" $ \v ->
        Member <$> v .:? "universityId"
               <*> v .:? "userId"
               <*> v .:? "firstName"
               <*> v .:? "fullFirstName"
               <*> v .:? "lastName"
               <*> v .:? "email"
               <*> v .:? "userType"
               <*> v .:? "fullName"
               <*> v .:? "officialName"
               <*> v .:? "homeEmail"
               <*> v .:? "title"
               <*> v .:? "gender"
               <*> v .:? "inUseFlag"
               <*> v .:? "jobTitle"
               <*> v .:? "phoneNumber"
               <*> v .:? "groupName"
               <*> v .:? "inactivationDate"
               <*> v .:? "studentCourseDetails"
               <*> v .:? "homeDepartment"
               <*> v .:? "touchedDepartments" .!= []
               <*> v .:? "affiliatedDepartments" .!= []

instance HasPayload Member where
    payloadFieldName _ = "member"

instance HasPayload [Member] where 
    payloadFieldName _ = "members"

--------------------------------------------------------------------------------

-- | Represents information about a student's status on a course.
data StudentCourseDetails = StudentCourseDetails {
    -- | The "student course join" code for a student in SITS.
    scdSCJCode :: Text,
    -- | The "student programme route" code for a student in SITS.
    scdSPRCode :: Text,
    -- | The code for the current level for this student course details.
    scdLevelCode :: Text,
    -- | The date that this student course details begins.
    scdBeginDate :: Date,
    -- | The date that this student course details ended.
    scdEndDate :: Maybe Date,
    -- | The date that this student course details is expected to end.
    scdExpectedEndDate :: Date,
    -- | The expected length of the course, in years.
    scdCourseYearLength :: Int,
    -- | Whether this is currently the most significant student course details -
    -- this will be `true` for only one student course details per student.
    scdMostSignificant :: Bool,
    -- | The SITS code for the reason for transfer.
    scdReasonForTransferCode :: Maybe Text,
    -- | An object representing the course for this student course details with
    -- properties `code`, `name` and `type`.
    scdCourse :: CourseDetails,
    -- | The relationships this member has with other members.
    scdRelationships :: M.Map Text [MemberRelationship],
    -- | The department which runs the course.
    scdDepartment :: DepartmentR
    -- TODO: more fields here, see
    -- https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/member/student-course-details-object
} deriving (Eq, Show)

instance FromJSON StudentCourseDetails where
    parseJSON = withObject "StudentCourseDetails" $ \v ->
        StudentCourseDetails <$> v .: "scjCode"
                             <*> v .: "sprCode"
                             <*> v .: "levelCode"
                             <*> v .: "beginDate"
                             <*> v .:? "endDate"
                             <*> v .: "expectedEndDate"
                             <*> v .: "courseYearLength"
                             <*> v .: "mostSignificant"
                             <*> v .:? "reasonForTransferCode"
                             <*> v .: "course"
                             <*> v .:? "relationships" .!= M.empty
                             <*> v .: "department"

-- | `mostSignificantCourse` @courses@ finds the most significant course in
-- @courses@ if there is one (which there should always be).
mostSignificantCourse :: [StudentCourseDetails] -> Maybe StudentCourseDetails
mostSignificantCourse xs = case [c | c <- xs, scdMostSignificant c] of
    [r] -> Just r
    _   -> Nothing

--------------------------------------------------------------------------------
