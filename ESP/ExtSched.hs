{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE RecordWildCards #-}
module ESP.ExtSched where

import Debug.Trace
import Control.Monad
import Data.List (intercalate)
import ESP.Common hiding (null)
import ESP.XMLOutput
import ESP.LP.Types
import System.Process
import System.IO
import Control.Concurrent
import qualified Control.Exception as C
-- import Text.PrettyPrint

import Text.ParserCombinators.Parsec

import Prelude hiding (lookup)

data ExtRoom = ERoom {eRoomId :: !Int, eX :: !Int, eY :: !Int, z :: !Int}
data ExtClass = EClass {eClassId :: !Int, eDuration :: !Int, eNumClassSections :: !Int,
	eNumClassTeachers :: !Int, eNumClassRooms :: !Int, eClassTeachers :: [Int],
	eGoodRooms :: [Int]}
data ExtSection = ESection {eSectionClass :: !Int, eSecNum :: !Int,
	eRoom :: !Int, eSecStart :: !Int}

data ExtSchedule = ExtSch {
	eTimeOffset :: !Int,
	eNumBlocks :: !Int,
	eNumRooms :: !Int,
	eNumClasses :: !Int,
	eNumTeachers :: !Int,
	eNumSections :: !Int,
	eRooms :: [ExtRoom],
	eTeachers :: [Int],
	eClasses :: [ExtClass],
	eSections :: [ExtSection]}

scheduleFile :: ExtSchedule -> [Int]
scheduleFile ExtSch{..} =
	[eNumBlocks, length eRooms, length eTeachers, length eClasses, length eSections] ++
		concatMap roomFile eRooms ++ eTeachers ++ concatMap classFile eClasses ++
		concatMap sectionFile eSections
	where	roomFile (ERoom rId x y z) = [rId, x, y, z]
		classFile (EClass cId dur nSecs nTs nRs cTs cRs)
			= [cId, dur, nSecs, length cTs, length cRs] ++ cTs ++ cRs
		sectionFile (ESection cId i r t) = [cId, i, r, t]

buildExtSchedule :: ESP -> Schedule -> ExtSchedule
buildExtSchedule ESP{..} Sched{..} =
	ExtSch blockOffset (size allBlocks) (size allRooms) (size allClasses) (size allTeachers) nClasses
		[maybe (ERoom rmId rmId 0 0) (\ (x, y, z) -> ERoom rmId x y z) pos
			| RInfo{..} <- elems allRooms]
		(keys allTeachers)
		[EClass cId duration (maybe 0 size (lookup cId unSched)) (size classTeachers)
			(size validRooms) (keys classTeachers) (keys validRooms) |
			CInfo{..} <- elems allClasses]
		[ESection cId i rId (tId - blockOffset) | (cId, ts) <- assocs unSched,
			(i, (tId, rId)) <- zip [0..] (assocs ts)]
	where	blockOffset = fst (findMin allBlocks)

java :: FilePath
java = "/usr/lib/jvm/java-6-sun/bin/java"

jScheduler :: FilePath
jScheduler = "/home/lowasser/esp-scheduling/"

simpleParser :: Int -> Parser [(Int, Int, Int)]
simpleParser off = (eof >> return []) <|> (do
	x <- many1 digit
	spaces
	y <- many1 digit
	spaces
	z <- many1 digit
	spaces
	liftM ((read x, read y, read z + off):) (simpleParser off))

runJava :: ExtSchedule -> IO (Either ParseError [(Int, Int, Int)])
runJava sched = do
	let f = scheduleFile sched
	let str = intercalate " " (map show f)
	writeFile "/tmp/java-data" str
	ans <- readProcess java ["-cp", jScheduler, "Schedule", "10"] str --readProcess' java ["-cp", jScheduler, "Schedule", "10"] Nothing str
	trace ans $ return (parse (simpleParser (eTimeOffset sched)) jScheduler ans)

runJava2 :: IO (Either ParseError [(Int, Int, Int)])
runJava2  = do
	str <- readFile "/tmp/java-data" 
	ans <- readProcess java ["-cp", jScheduler, "Schedule", "10"] str --readProcess' java ["-cp", jScheduler, "Schedule", "10"] Nothing str
	trace ans $ return (parse (simpleParser 61) jScheduler ans)

-- compileJava :: IO ()
-- compileJava = do	readProcess javac [jScheduler ++ ".java"] ""
-- 			return ()

readProcess'
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> Maybe FilePath		-- ^ possibly a working directory
    -> String                   -- ^ standard input
    -> IO String -- ^ exitcode, stdout, stderr
readProcess' cmd args path input = do
    (Just inh, Just outh, _, pid) <- traceShow path $
        createProcess (proc cmd args){ cwd = path, std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = Inherit }

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- hGetContents outh
    _ <- forkIO $ C.evaluate (length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
--     err  <- hGetContents errh
--     _ <- forkIO $ C.evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh

    -- wait on the process
    waitForProcess pid

    return (out)	
