--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.API where 

--------------------------------------------------------------------------------

import Data.ByteString as BS
import Data.Text
import Data.Proxy

import Text.XML as C

import Servant.API
import Servant.Client

import Warwick.Sitebuilder.Atom
import Warwick.Sitebuilder.PageUpdate

--------------------------------------------------------------------------------
        
instance MimeUnrender ATOM () where 
    mimeUnrender _ input = case parseLBS def input of 
        Left ex -> Left $ show ex
        Right d -> pure ()

type SitebuilderAuth = BasicAuth "" ()

type SitebuilderAPI =
      SitebuilderAuth :>
      "edit" :>
      "atom" :>
      "atom.htm" :>
      QueryParam "page" Text :>
      QueryParam "type" Text :>
      ReqBody '[ATOM] PageUpdate :>
      Put '[ATOM] ()

-- | A proxy value for the 'SitebuilderAPI' type.
sitebuilder :: Proxy SitebuilderAPI
sitebuilder = Proxy

--------------------------------------------------------------------------------

editPage :: BasicAuthData -> Maybe Text -> Maybe Text -> PageUpdate -> ClientM ()

editPage = client sitebuilder

--------------------------------------------------------------------------------