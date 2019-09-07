--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.MonitoringPoints where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Common
import Warwick.Tabula.Payload.Note

--------------------------------------------------------------------------------

data MonitoringPoint = MonitoringPoint {
    mpName :: Text,
    mpStartWeek :: Maybe Int,
    mpEndWeek :: Maybe Int,
    mpTerm :: Text,
    mpStartDate :: Maybe Date,
    mpEndDate :: Maybe Date
} deriving (Eq, Show)

instance FromJSON MonitoringPoint where 
    parseJSON = withObject "MonitoringPoint" $ \obj ->
        MonitoringPoint <$> obj .: "name"
                        <*> obj .:? "startWeek"
                        <*> obj .:? "endWeek"
                        <*> obj .: "term"
                        <*> obj .:? "startDate"
                        <*> obj .:? "endDate"

--------------------------------------------------------------------------------

-- | Enumerates monitoring point states.
data MonitoringPointState 
    -- | The student attended the monitoring point.
    = AttendedMP 
    -- | The student was absent, which was authorised.
    | AuthorisedAbsentMP
    -- | The student was absent, which was unauthorised.
    | UnauthorisedAbsentMP
    -- | The monitoring point is not yet recorded.
    | NotRecordedMP
    deriving (Eq, Show)

instance FromJSON MonitoringPointState where 
    parseJSON (String "attended") = pure AttendedMP
    parseJSON (String "authorised") = pure AuthorisedAbsentMP
    parseJSON (String "unauthorised") = pure UnauthorisedAbsentMP
    parseJSON (String "not-recorded") = pure NotRecordedMP
    parseJSON _ = fail "Not a valid MonitoringPointState"

--------------------------------------------------------------------------------

data MonitoringPointAttendance = MonitoringPointAttendance {
    mpaPoint :: MonitoringPoint,
    mpaState :: MonitoringPointState,
    mpaAutoCreated :: Maybe Bool,
    mpaUpdatedDate :: Maybe DateTime,
    mpaUpdatedBy :: Maybe Text,
    mpaNote :: Maybe AttendanceNote
} deriving (Eq, Show)

instance FromJSON MonitoringPointAttendance where 
    parseJSON = withObject "MonitoringPointAttendance" $ \obj ->
        MonitoringPointAttendance <$> obj .: "point"
                                  <*> obj .: "state"
                                  <*> obj .:? "autoCreated"
                                  <*> obj .:? "updatedDate"
                                  <*> obj .:? "updatedBy"
                                  <*> obj .:? "note"

--------------------------------------------------------------------------------