--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.SSO.SearchParams where 

--------------------------------------------------------------------------------

import Data.Text

import Warwick.SSO.User

--------------------------------------------------------------------------------

-- | Represents configurations for SSO user search queries.
data SearchParams = SearchParams {
    -- | If set, the university ID to search for.
    ssoSearchID :: Maybe Text,
    -- | If set, the username to search for.
    ssoSearchUsername :: Maybe Text,
    -- | If set, the email address to search for.
    ssoSearchEmail :: Maybe Text,
    -- | If set, the given name to search for.
    ssoSearchGivenName :: Maybe Text,
    -- | If set, the surname to search for.
    ssoSearchSurname :: Maybe Text,
    -- | If set, a department code to search for.
    ssoSearchDeptCode :: Maybe Text,
    -- | If set, a department's short name to search for.
    ssoSearchDeptShort :: Maybe Text,
    -- | If set, the department name to search for.
    ssoSearchDeptName :: Maybe Text, 
    -- | If set, the organisational unit to search for.
    ssoSearchOrgUnit :: Maybe Text,
    -- | If set, a value indicating what types of users to search for.
    ssoSearchUserClass :: Maybe UserClass,
    -- | If set, a value indicating whether to search for disabled accounts.
    ssoSearchLoginDisabled :: Maybe Bool,
    -- | If set, a value indicating whether to search for accounts whose 
    -- password is expired.
    ssoSearchPasswordExpired :: Maybe Bool, 
    -- | If set, a value indicating whether to search for primary accounts.
    ssoSearchPrimaryAccount :: Maybe Bool
}

-- | `defaultSearch` is a `SearchParams` value with all fields set to 
-- `Nothing`. Useful to construct a query with record updates.
defaultSearch :: SearchParams
defaultSearch = SearchParams {
    ssoSearchID = Nothing,
    ssoSearchUsername = Nothing,
    ssoSearchEmail = Nothing,
    ssoSearchGivenName = Nothing,
    ssoSearchSurname = Nothing,
    ssoSearchDeptCode = Nothing,
    ssoSearchDeptShort = Nothing,
    ssoSearchDeptName = Nothing,
    ssoSearchOrgUnit = Nothing,
    ssoSearchUserClass = Nothing,
    ssoSearchLoginDisabled = Nothing,
    ssoSearchPasswordExpired = Nothing,
    ssoSearchPrimaryAccount = Nothing
}

--------------------------------------------------------------------------------
    