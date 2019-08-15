--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Module where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Tabula.Payload.Department
import Warwick.Tabula.Types

--------------------------------------------------------------------------------

-- | Represents information about a module.
data Module = Module {
    -- | A value indicating whether the module is currently active.
    mActive :: Bool,
    -- | Information about the department responsible for the module.
    mAdminDepartment :: DepartmentR,
    -- | The module code.
    mCode :: Text,
    -- | The name of the module.
    mName :: Text
} deriving Show

instance FromJSON Module where 
    parseJSON = withObject "Module" $ \obj ->
        Module <$> obj .: "active" 
               <*> obj .: "adminDepartment"
               <*> obj .: "code"
               <*> obj .: "name"

instance HasPayload Module where 
    payloadFieldName _ = "module"

--------------------------------------------------------------------------------