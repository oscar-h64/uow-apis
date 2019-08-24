--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

-- | This module should not exist.
module Warwick.SSO.YesNo where 

--------------------------------------------------------------------------------

import Servant.API

import Warwick.XML

--------------------------------------------------------------------------------

-- | Some people do not know what a boolean is. We use a newtype so that the
-- runtime representation is the same as a `Bool` and it is a no-op to convert
-- between `Bool` values and this abomination.
newtype YesNo = YesNo { unAbominate :: Bool } 
    deriving (Eq, Ord, Enum, Bounded, Show)

-- | "smart" constructor for @YesNo True@.
yes :: YesNo
yes = YesNo True 

-- | "smart" constructor for @YesNo False@.
no :: YesNo 
no = YesNo False

instance ToHttpApiData YesNo where 
    toQueryParam (YesNo True)  = "Yes"
    toQueryParam (YesNo False) = "No"

instance FromByteString YesNo where 
    fromByteString "Yes" = yes 
    fromByteString "No"  = no 
    fromByteString _     = error "Not a yes/no value"

--------------------------------------------------------------------------------
