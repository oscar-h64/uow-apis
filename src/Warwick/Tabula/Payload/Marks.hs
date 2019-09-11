--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Marks where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

--------------------------------------------------------------------------------

data FeedbackItem = FeedbackItem {
    fiId :: Text,
    fiMark :: Maybe Text,
    fiGrade :: Maybe Text,
    fiFeedback :: Maybe Text
} deriving (Eq, Show)

instance ToJSON FeedbackItem where 
    toJSON FeedbackItem{..} =
        object [ "id" .= fiId
               , "mark" .= fiMark
               , "grade" .= fiGrade
               , "feedback" .= fiFeedback
               ]

data Marks = Marks {
    mStudents :: [FeedbackItem]
} deriving (Eq, Show)

instance ToJSON Marks where 
    toJSON Marks{..} = 
        object [ "students" .= mStudents ]

--------------------------------------------------------------------------------