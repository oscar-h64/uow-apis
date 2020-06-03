-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module CmdArgs (
    SitebuilderOpts(..),
    Command(..),
    parseCmdLineArgs
) where 

--------------------------------------------------------------------------------

import Data.Text 

import Options.Applicative

--------------------------------------------------------------------------------

data SitebuilderOpts 
    = EditPage {
        cPage :: Text,
        cFile :: FilePath,
        cComment :: Maybe Text
    }
    | UploadFile {
        cPage :: Text,
        cFile :: FilePath,
        cSlug :: Maybe Text
    }

editPageP :: Parser SitebuilderOpts 
editPageP = EditPage  
    <$> strOption (long "page" <> metavar "PAGE")
    <*> strOption (long "file" <> metavar "FILE")
    <*> optional (strOption (long "comment"))

uploadFileP :: Parser SitebuilderOpts 
uploadFileP = UploadFile 
    <$> strOption (long "page" <> metavar "PAGE")
    <*> strOption (long "file" <> metavar "FILE")
    <*> optional (strOption (long "name"))

sitebuilderP :: Parser Command 
sitebuilderP = fmap SitebuilderCmd $ subparser $
    command "edit" (info editPageP (progDesc "Edit a file.")) <> 
    command "upload" (info uploadFileP (progDesc "Upload a file."))

--------------------------------------------------------------------------------

-- data TabulaOpts 

--------------------------------------------------------------------------------

data Command 
    = SitebuilderCmd SitebuilderOpts 
    -- | TabulaCmd TabulaOpts

commandP :: Parser Command 
commandP = subparser $ 
    command "sitebuilder" (info sitebuilderP (progDesc "Sitebuilder commands"))

opts :: ParserInfo Command 
opts = info (commandP <**> helper) idm

-- | 'parseCmdLineArgs' is a computation which parses the command-line
-- arguments into a 'Command' value.
parseCmdLineArgs :: IO Command 
parseCmdLineArgs = execParser opts

-------------------------------------------------------------------------------