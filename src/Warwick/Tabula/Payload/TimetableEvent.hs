--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright (c) Michael B. Gale (m.gale@warwick.ac.uk)                       --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.TimetableEvent (
    TimetableEvent(..)
) where 

--------------------------------------------------------------------------------

import Data.Text

--------------------------------------------------------------------------------

data TimetableEvent = TimetableEvent {
    -- | A unique identifier for the timetable event. For Syllabus+ events, 
    -- this is generated from an MD5 hex of the name, start time, end time, 
    -- day of week, location name and context.
    eventID :: Text,
    -- | The name of the timetable event, e.g. CS118L.
    eventName :: Text,
    -- | The human-readable of the timetable event. Typically empty for 
    -- Syallabus+ events.
    eventTitle :: Maybe Text,
    -- | A description of the timetable event.
    eventDescription :: Text,
    -- | 
    eventType :: Text
}

--------------------------------------------------------------------------------
