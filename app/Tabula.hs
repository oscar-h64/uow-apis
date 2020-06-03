-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Tabula ( tabulaMain ) where

-------------------------------------------------------------------------------

import Control.Monad 
import Control.Monad.Extra
import Control.Monad.IO.Class

import qualified Data.ByteString as BS (length)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Data.Text (pack, unpack)

import System.Console.AsciiProgress
import System.Directory
import System.FilePath
import System.IO

import Text.Read (readMaybe)

import Warwick.Tabula
import Warwick.Tabula.Attachment
import Warwick.Tabula.Member
import Warwick.Tabula.StudentAssignment

import CmdArgs 

--------------------------------------------------------------------------------

-- untar :: FilePath -> FilePath -> IO ()
-- untar zf dir = S.shelly $ S.silently $ S.errExit False $ do
--     r <- S.run "tar" ["-xf", T.pack zf, "-C", T.pack dir]
--     e <- S.lastExitCode
--     d <- S.lastStderr

--     S.liftIO $ do
--         if e == 0 then do
--             --setSGR [SetColor Foreground Dull Green]
--             putStr "Unpacked."
--             --setSGR [Reset]
--         else do
--             --setSGR [SetColor Foreground Vivid Red]
--             putStr $ "Unpack failed (" ++ show e ++ ")."
--             --setSGR [Reset]

-- unzip :: FilePath -> FilePath -> IO ()
-- unzip zf dir = S.shelly $ S.silently $ S.errExit False $ do
--     r <- S.run "unzip" [T.pack zf, "-d", T.pack dir]
--     e <- S.lastExitCode
--     d <- S.lastStderr

--     S.liftIO $ do
--         if e == 0 then do
--             --setSGR [SetColor Foreground Dull Green]
--             putStr "Unzipped."
--             --setSGR [Reset]
--         else do
--             --setSGR [SetColor Foreground Vivid Red]
--             putStr $ "Unzip failed (" ++ show e ++ ")."
--             --setSGR [Reset]

unpackSubmission :: FilePath -> FilePath -> IO ()
unpackSubmission _ file = do
    case takeExtension file of
            ".zip" -> do
                --putStr $ "Unzipping " ++ file ++ "... "
                -- unzip file dir
                pure ()
            ".gz" -> do
                --putStr $ "Unpacking " ++ file ++ "... "
                -- untar file dir
                pure ()
            _ -> putStr $ "Unpack not supported."

--------------------------------------------------------------------------------

-- | Pretty-prints an assignment for the assignment selection menu.
ppAssignment :: (Assignment, Int) -> IO ()
ppAssignment (Assignment {..}, idx) = do
    putStr (show idx)
    putStr ". "
    putStr assignmentName
    when assignmentArchived $ putStr " (Archived)"
    putStr " - "
    putStr (show assignmentSubmissions)
    putStrLn " submission(s)"

-- | Prompts the user to select a coursework within the range.
promptID :: Int -> IO Int
promptID len = do
    putStr "Select coursework [0.."
    putStr (show $ len-1)
    putStr "]: "

    r <- readMaybe <$> getLine

    case r of
        Nothing -> do
            putStrLn "Not a valid integer."
            promptID len
        Just idx | idx >= len -> do
                    putStrLn "Out of range."
                    promptID len
                 | otherwise ->
                    return idx

-- | Downloads all coursework submissions for an assignment.
downloadSubmissions :: ModuleCode 
                    -> Assignment 
                    -> Bool 
                    -> Bool 
                    -> Tabula ()
downloadSubmissions mc cwk up pdf = do
    let aid = assignmentID cwk
        anm = assignmentName cwk
        dir = "./submissions-" ++ show aid

    -- get a list of all submissions for the assignment
    TabulaOK{..} <- listSubmissions mc aid

    -- create a directory for the submissions, if there isn't one yet
    liftIO $ do
        putStrLn $ "Downloading submissions for " ++ anm ++ "..."

        -- create the directory if it doesn't exist
        unlessM (doesDirectoryExist dir) $ createDirectory dir

    -- download every submission
    forM_ (HM.toList tabulaData) $ \(sid, subm) ->
            case subm of
                Nothing  -> liftIO $ putStrLn $ sid ++ " has not submitted anything."
                Just sub -> forM_ (submissionAttachments sub) $ \att -> do
                    let
                        subDir = dir </> sid
                        subFile = subDir </> attachmentFilename att
                        subExt = takeExtension (attachmentFilename att)
                    if ((subExt /= ".pdf") && pdf) then do
                        liftIO $ putStrLn ("Skipping submission for " ++ sid ++ " (" ++ attachmentFilename att ++ " is not a PDF) ... ")
                    else do
                        liftIO $ do
                            putStrLn $ "Downloading submission for " ++ sid ++ " ... "
                            unlessM (doesDirectoryExist subDir) $ createDirectory subDir
                        downloadSubmissionWithCallbacks
                            sid
                            mc
                            (assignmentID cwk)
                            (submissionID sub)
                            (attachmentFilename att)
                            subFile
                            Callbacks {
                                onWrapper = displayConsoleRegions,
                                onLength = \l -> newProgressBar def {
                                                pgTotal = fromIntegral l,
                                                pgWidth = 100,
                                                pgOnCompletion = Just "Downloaded."
                                           },
                                onUpdate = \pb bs -> tickN pb (BS.length bs),
                                onComplete = complete
                            }
                        liftIO $ do
                            --putStr "Done. "
                            when up $ unpackSubmission subDir subFile
                            --putStrLn ""

selectAssignment :: APIConfig -> ModuleCode -> String -> Bool -> Bool -> IO ()
selectAssignment cfg mc ay up pdf = do
    r <- withTabula Live cfg $ do
        TabulaOK {..} <- listAssignments mc (Just ay)

        case tabulaData of
            []  -> liftIO $ putStrLn "There are no assignments for this module."
            [x] -> downloadSubmissions mc x up pdf
            xs  -> do
                idx <- liftIO $ do
                    mapM_ ppAssignment $ zip tabulaData [0..]
                    promptID (length tabulaData)

                downloadSubmissions mc (xs !! idx) up pdf

    -- handle errors
    case r of
        Left err -> putStrLn $ "Communication error:\n" ++ show err
        Right r -> putStrLn "Program ran successfully."

concernAssignments :: String 
                   -> [StudentAssignment] 
                   -> [StudentAssignment]
concernAssignments ay = 
    filter $ \StudentAssignment{..} -> case studentAssignmentSubmission of
        -- if there is no submission, would it be late now?
        Nothing -> studentAssignmentLate && studentAssignmentAcademicYear == ay
        -- otherwise, if there is a submission, was the submission late?
        Just StudentAssignmentSubmission{..} ->
            studentAssignmentSubmissionLate && studentAssignmentAcademicYear == ay

--------------------------------------------------------------------------------

tabulaMain :: APIConfig -> TabulaOpts -> IO () 
tabulaMain cfg opts = do 
    -- disable buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin LineBuffering

    case opts of
        DownloadSubmissions{..} ->
            selectAssignment cfg 
                             tabulaOptsModuleCode 
                             (unpack tabulaOptsAcademicYear)
                             tabulaOptsUnpack 
                             tabulaOptsOnlyPDF
        Tutees{..} -> do
            let ay = unpack tabulaOptsAcademicYear
            putStr "User ID: "
            uid <- getLine

            r <- withTabula Live cfg $ do
                TabulaOK{..} <- listRelationships uid

                rs <- forM (relationshipsOfType "personalTutor" tabulaData) $ \r -> do
                    TabulaOK{..} <- retrieveMembers (map (pack . relationshipEntryUniversityID) $ relationshipStudents r) ["member.fullName"]

                    forM (HM.toList tabulaData) $ \(k,v) -> do
                        liftIO $ putStrLn (fromJust (memberFullName v) ++ "(" ++ unpack k ++ ")")
                        TabulaAssignmentOK {..} <- personAssignments (unpack k) (Just ay)
                        let hls = concernAssignments ay $ historicAssignments tabulaAssignmentData
                            els = concernAssignments ay $ enrolledAssignments tabulaAssignmentData

                        return (memberFullName v, length els, length hls)
                liftIO $ mapM_ print rs
                return ()

            print r

-------------------------------------------------------------------------------
