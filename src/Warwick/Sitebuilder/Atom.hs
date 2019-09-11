--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.Atom where 

--------------------------------------------------------------------------------

import Data.XML.Types as XML

import Network.HTTP.Media ((//))

import Text.XML as C

import Servant.API

--------------------------------------------------------------------------------

data ATOM 

instance Accept ATOM where 
    contentType _ = "application" // "atom+xml"

elementToDoc :: XML.Element -> C.Document
elementToDoc el = case fromXMLDocument $ XML.Document (Prologue [] Nothing []) el [] of
    Left err -> error $ show err 
    Right doc -> doc

--------------------------------------------------------------------------------