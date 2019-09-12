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
 :<|> SitebuilderAuth :>
      "edit" :>
      "atom" :>
      "file.htm" :>
      QueryParam "page" Text :> 
      Header "Slug" Text :>
      Header "Content-Type" Text :>
      ReqBody '[OctetStream] BS.ByteString :>
      Post '[JSON] NoContent

-- | A proxy value for the 'SitebuilderAPI' type.
sitebuilder :: Proxy SitebuilderAPI
sitebuilder = Proxy

--------------------------------------------------------------------------------

editPage :: BasicAuthData -> Maybe Text -> Maybe Text -> PageUpdate -> ClientM ()

uploadFile :: BasicAuthData
           -> Maybe Text -- ^ The page path
           -> Maybe Text -- ^ The "Slug" header
           -> Maybe Text -- ^ The "Content-Type" header
           -> ByteString -- ^ The file contents
           -> ClientM NoContent

editPage 
 :<|> uploadFile = client sitebuilder

--------------------------------------------------------------------------------