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
import Warwick.Tabula.MemberSearchFilter (CourseType)

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
    | Enrolments {
        tabulaOptsAcademicYear :: Text,
        tabulaOptsDepartments :: [Text],
        tabulaOptsYearGroup :: [Int],
        tabulaOptsCourseGroups :: [CourseType]
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

enrolmentsP :: Parser TabulaOpts
enrolmentsP = Enrolments
    <$> strOption (long "academic-year" <> help "The academic year")
    <*> some (strOption (long "department" <> help "Departments to filter by"))
    <*> many (option auto (long "year" <> help "Year groups to filter by"))
    <*> many (option auto (long "course-type" <> help "Course types to filter by"))

tabulaP :: Parser Command
tabulaP = fmap TabulaCmd $ subparser $
    command "download" (info downloadSubmissionsP (progDesc "Download coursework submissions."))
 <> command "tutees" (info tuteesP (progDesc "View information about tutees"))
 <> command "enrolments" (info enrolmentsP (progDesc "View enrolment information"))

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