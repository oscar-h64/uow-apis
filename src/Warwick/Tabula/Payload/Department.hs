--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Department where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text 

--------------------------------------------------------------------------------

-- | Represents (reduced) information about a department.
data DepartmentR = DepartmentR {
    -- | The unique department code.
    dCode :: Text, 
    -- | The department's name.
    dName :: Text
} deriving Show

instance FromJSON DepartmentR where 
    parseJSON = withObject "Department (reduced)" $ \obj ->
        DepartmentR <$> obj .: "code" 
                    <*> obj .: "name"

--------------------------------------------------------------------------------
