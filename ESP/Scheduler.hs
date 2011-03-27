-- | The main module.
module ESP.Scheduler where

import Data.Maybe
import Control.Monad
import Control.DeepSeq
import Control.Concurrent
import ESP.LP
import Control.Parallel
import Data.Unamb
-- import ESP.LP
import Data.LinearProgram
import Data.LinearProgram.GLPK
import ESP.XMLParser
import ESP.XMLOutput
-- import System.Time
-- import Data.Time.Clock
import System.CPUTime
import ESP.ExtSched
import System.IO
-- import ESP.LPPass2
-- import Data.Monoid
-- import Text.ParserCombinators.Parsec.Prim
import Debug.Trace
-- import Text.XML.HXT.DOM.ShowXml

import System.Environment

main :: IO ()
main = do	input <- getContents
		(outfil:_) <- getArgs
		main' input outfil

main' :: String -> String -> IO ()
main' input outfil = do	let (doc, cNames, rNames, tNames) = join deepseq $ processDocument input
			let linProg = espProgram doc
			let linProg' = espProgram' doc
			writeLP "/tmp/esp_program.cplex" linProg
			t <- getCPUTime
			(code, msol) <-
				trace "Linear program constructed" $
					glpSolveVars mipDefaults{fpHeur = True, ppTech = AllPre, btTech=ProjHeur, brTech = HybridP,
					cuts = [GMI, MIR, Cov, Clq], tmLim = 600, msgLev = MsgAll, mipGap = 0.07} linProg
			t' <- getCPUTime
			putStrLn (show $ fromIntegral (t' - t) / (10.0 ^ 12))
			case msol of
				Nothing	-> writeFile outfil (show code)
				Just sol -> do	let extSch = trace "Passing solution to Java annealing" $ buildExtSchedule doc (buildSch $ readSol sol)
						either (fail . show) (writeFile outfil . formatSchedule cNames rNames tNames . buildSch)
							=<< runJava extSch
