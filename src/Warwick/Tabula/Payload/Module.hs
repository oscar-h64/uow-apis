--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Module where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.HashMap.Lazy as HM

import Warwick.Tabula.Payload.Department
import Warwick.Tabula.Payload.BaseModule
import Warwick.Tabula.Types
import Warwick.Tabula.JSON

--------------------------------------------------------------------------------

-- | Represents information about a module.
data Module = Module {
    -- | The base module details.
    mBaseModule :: BaseModule,
    -- | Information about the department responsible for the module.
    mAdminDepartment :: Maybe DepartmentR
} deriving (Eq, Show)

instance FromJSON Module where 
    parseJSON = withObject "Module" $ \obj ->
        Module <$> parseJSON (Object obj) 
               <*> obj .:? "adminDepartment"

instance ToJSON Module where 
    toJSON Module{..} = compactObject 
        [ "active" .=? mActive mBaseModule
        , "code" .= mCode mBaseModule
        , "name" .= mName mBaseModule
        , "adminDepartment" .=? mAdminDepartment
        ] 

instance HasPayload Module where 
    payloadFieldName _ = "module"

instance HasPayload [Module] where 
    payloadFieldName _ = "modules"

--------------------------------------------------------------------------------