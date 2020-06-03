-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module CmdArgs (
    SitebuilderOpts(..),
    TabulaOpts(..),
    Command(..),
    parseCmdLineArgs
) where 

--------------------------------------------------------------------------------

import Data.Text 

import Options.Applicative

import Warwick.Tabula.Types

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
    deriving (Eq, Show)

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

data TabulaOpts
    = DownloadSubmissions {
        tabulaOptsModuleCode :: ModuleCode,
        tabulaOptsAcademicYear :: Text,
        tabulaOptsUnpack :: Bool,
        tabulaOptsOnlyPDF :: Bool
    }
    | Tutees {
        tabulaOptsAcademicYear :: Text
    }
    deriving (Eq, Show) 

mc :: ReadM ModuleCode
mc = ModuleCode . pack <$> str

downloadSubmissionsP :: Parser TabulaOpts
downloadSubmissionsP = DownloadSubmissions 
    <$> argument mc ( metavar "MODULE" <> 
                      help "The module code (e.g. cs141)"
                    )
    <*> strOption (long "year" <> help "The academic year")
    <*> switch ( long "unpack" <> 
                 help "Unpack submissions automatically."
               )
    <*> switch ( long "only-pdf" <> 
                 help "Only download PDFs"
               )

tuteesP :: Parser TabulaOpts
tuteesP = Tutees
    <$> strOption (long "year" <> help "The academic year")

tabulaP :: Parser Command
tabulaP = fmap TabulaCmd $ subparser $
    command "download" (info downloadSubmissionsP (progDesc "Download coursework submissions."))
 <> command "tutees" (info tuteesP (progDesc "View information about tutees"))

--------------------------------------------------------------------------------

data Command 
    = SitebuilderCmd SitebuilderOpts 
    | TabulaCmd TabulaOpts
    deriving (Eq, Show)

commandP :: Parser Command 
commandP = subparser $ 
    command "sitebuilder" (info sitebuilderP (progDesc "Sitebuilder commands"))
 <> command "tabula" (info tabulaP (progDesc "Tabula commands"))

opts :: ParserInfo Command 
opts = info (commandP <**> helper) idm

-- | 'parseCmdLineArgs' is a computation which parses the command-line
-- arguments into a 'Command' value.
parseCmdLineArgs :: IO Command 
parseCmdLineArgs = execParser opts

-------------------------------------------------------------------------------