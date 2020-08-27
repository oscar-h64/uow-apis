-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Tabula.Member (
    MemberRelationship(..),
    Member(..),
    EnrolmentStatus(..),
    StudentCourseYearDetails(..),
    StudentCourseDetails(..),
    mostSignificantCourse,
    currentEnrolmentStatus
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

data ModeOfAttendance = ModeOfAttendance {
    -- | The status code (from SITS).
    attendanceCode :: Text,
    -- | A human-readable description of the status code.
    attendanceName :: Text
} deriving (Eq, Show)

instance FromJSON ModeOfAttendance where 
    parseJSON = withObject "ModeOfAttendance" $ \obj ->
        ModeOfAttendance <$> obj .: "code" <*> obj .: "name"

-- | Represents the status of a student's enrolment on a course.
data EnrolmentStatus = EnrolmentStatus {
    -- | The status code (from SITS).
    enrolmentStatusCode :: Text,
    -- | A human-readable description of the status code.
    enrolmentStatusName :: Text
} deriving (Eq, Show)

instance FromJSON EnrolmentStatus where 
    parseJSON = withObject "EnrolmentStatus" $ \obj ->
        EnrolmentStatus <$> obj .: "code" <*> obj .: "name"

-- | Represents information about a year within a student's course.
data StudentCourseYearDetails = StudentCourseYearDetails {
    scydSequenceNumber :: Text,
    -- | Information about the mode of attendance.
    scydModeOfAttendance :: ModeOfAttendance,
    -- | A code identifying the study level.
    scydStudyLevel :: Text,
    -- | The academic year to which these details relate.
    scydAcademicYear :: Text,
    -- | The department in which the student is enroled.
    scydEnrolmentDepartment :: DepartmentR,
    -- | The enrolment status for this course year.
    scydEnrolmentStatus :: EnrolmentStatus,
    -- | The year of study.
    scydYearOfStudy :: Int
} deriving (Eq, Show)

instance FromJSON StudentCourseYearDetails where 
    parseJSON = withObject "StudentCourseYearDetails" $ \obj -> 
        StudentCourseYearDetails <$> obj .: "sceSequenceNumber"
                                 <*> obj .: "modeOfAttendance"
                                 <*> obj .: "studyLevel"
                                 <*> obj .: "academicYear"
                                 <*> obj .: "enrolmentDepartment"
                                 <*> obj .: "enrolmentStatus"
                                 <*> obj .: "yearOfStudy"

-- | Represents information about a student's status on a course.
data StudentCourseDetails = StudentCourseDetails {
    -- | The "student course join" code for a student in SITS.
    scdSCJCode :: Maybe Text,
    -- | The "student programme route" code for a student in SITS.
    scdSPRCode :: Maybe Text,
    -- | The code for the current level for this student course details.
    scdLevelCode :: Maybe Text,
    -- | The date that this student course details begins.
    scdBeginDate :: Maybe Date,
    -- | The date that this student course details ended.
    scdEndDate :: Maybe Date,
    -- | The date that this student course details is expected to end.
    scdExpectedEndDate :: Maybe Date,
    -- | The expected length of the course, in years.
    scdCourseYearLength :: Maybe Int,
    -- | Whether this is currently the most significant student course details -
    -- this will be `true` for only one student course details per student.
    scdMostSignificant :: Maybe Bool,
    -- | The SITS code for the reason for transfer.
    scdReasonForTransferCode :: Maybe Text,
    -- | An object representing the course for this student course details with
    -- properties `code`, `name` and `type`.
    scdCourse :: Maybe CourseDetails,
    -- | The relationships this member has with other members.
    scdRelationships :: M.Map Text [MemberRelationship],
    -- | The department which runs the course.
    scdDepartment :: Maybe DepartmentR,
    -- | Information about the student's years.
    scdCourseYearDetails :: [StudentCourseYearDetails]
    -- TODO: more fields here, see
    -- https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/member/student-course-details-object
} deriving (Eq, Show)

instance FromJSON StudentCourseDetails where
    parseJSON = withObject "StudentCourseDetails" $ \v ->
        StudentCourseDetails <$> v .:? "scjCode"
                             <*> v .:? "sprCode"
                             <*> v .:? "levelCode"
                             <*> v .:? "beginDate"
                             <*> v .:? "endDate"
                             <*> v .:? "expectedEndDate"
                             <*> v .:? "courseYearLength"
                             <*> v .:? "mostSignificant"
                             <*> v .:? "reasonForTransferCode"
                             <*> v .:? "course"
                             <*> v .:? "relationships" .!= M.empty
                             <*> v .:? "department"
                             <*> v .:? "studentCourseYearDetails" .!= []

-- | `mostSignificantCourse` @courses@ finds the most significant course in
-- @courses@ if there is one (which there should always be).
mostSignificantCourse :: [StudentCourseDetails] -> Maybe StudentCourseDetails
mostSignificantCourse xs = case [c | c <- xs, scdMostSignificant c == Just True] of
    [r] -> Just r
    _   -> Nothing

-- | 'currentEnrolmentStatus' @academicYear courseDetails@ tries to retrieve
-- the 'EnrolmentStatus' value for the academic year identified by 
-- @academicYear@ from @courseDetails@.
currentEnrolmentStatus :: Text -> [StudentCourseDetails] -> Maybe EnrolmentStatus
currentEnrolmentStatus _ [] = Nothing
currentEnrolmentStatus year [x] = 
    case Prelude.filter ((==) year . scydAcademicYear) years of 
        [] -> Nothing 
        (y:ys) -> Just $ scydEnrolmentStatus y
    where years = scdCourseYearDetails x
currentEnrolmentStatus year (x:xs) 
    | scdMostSignificant x == Just True = currentEnrolmentStatus year [x]
    | otherwise = currentEnrolmentStatus year xs

--------------------------------------------------------------------------------
