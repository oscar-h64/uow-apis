--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.SSO.API where 

--------------------------------------------------------------------------------

import Data.Text
import Data.Proxy

import Servant.API
import Servant.Client

import Warwick.XML
import Warwick.SSO.User
import Warwick.SSO.YesNo

--------------------------------------------------------------------------------

type SSOApi =
    "userSearch.htm" :>
    QueryParam "wsos_api_key" Text :>
    QueryParam "f_warwickuniid" Text :>
    QueryParam "f_cn" Text :> 
    QueryParam "f_mail" Text :>
    QueryParam "f_givenName" Text :>
    QueryParam "f_sn" Text :>
    QueryParam "f_warwickdeptcode" Text :>
    QueryParam "f_deptshort" Text :>
    QueryParam "f_department" Text :>
    QueryParam "f_ou" Text :>
    QueryParam "f_warwickitsclass" UserClass :>
    QueryParam "f_logindisabled" Bool :>
    QueryParam "f_passwordexpired" Bool :> 
    QueryParam "f_warwickprimary" YesNo :>
    Get '[XML] [User]

-- | A proxy value for the `SSOApi` type.
sso :: Proxy SSOApi
sso = Proxy

--------------------------------------------------------------------------------

userSearch ::
    Maybe Text -> 
    Maybe Text -> 
    Maybe Text -> 
    Maybe Text -> 
    Maybe Text -> 
    Maybe Text -> 
    Maybe Text -> 
    Maybe Text ->
    Maybe Text -> 
    Maybe Text ->
    Maybe UserClass ->
    Maybe Bool ->
    Maybe Bool ->
    Maybe YesNo ->
    ClientM [User]

userSearch = client sso

--------------------------------------------------------------------------------
    