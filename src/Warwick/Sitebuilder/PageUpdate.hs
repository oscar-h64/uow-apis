--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.PageUpdate where 

--------------------------------------------------------------------------------

import Data.Text
import Data.XML.Types 

import Text.Atom.Feed
import Text.Atom.Feed.Export
import Text.XML

import Servant.API

import Warwick.Sitebuilder.Atom

--------------------------------------------------------------------------------

data PageUpdate = PageUpdate {
    puContents :: Text,
    puChangeNote :: Text
}

instance MimeRender ATOM PageUpdate where 
    mimeRender _ PageUpdate{..} = 
        renderLBS def $ 
        elementToDoc $ 
        xmlEntry $ 
        (nullEntry "" (TextString "") "") {
            entryContent = Just $ HTMLContent puContents,
            entryAttrs = [
                ("xmlns:sitebuilder", [
                    ContentText "http://go.warwick.ac.uk/elab-schemas/atom"
                ])
            ],
            entryOther = [
                xmlTextContent "sitebuilder:edit-comment" (TextString puChangeNote)
            ]
        } 

--------------------------------------------------------------------------------
