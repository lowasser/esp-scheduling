{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE PackageImports, Arrows, BangPatterns, TupleSections, RecordWildCards, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module ESP.Common (module Control.Monad, module Data.IntMap, module Data.Maybe, module Data.Either, -- module ESP.Algebra,
	(<$>), (&&&), second, Teacher, Room, Class, Time, Resource, IntSet, TeacherSet,
	RoomSet, ClassSet, TimeSet, ResourceSet, TeacherMap, RoomMap, ClassMap, TimeMap, ResourceMap, TeacherInfo(..),
	ClassInfo(..), ResourceInfo(..), RoomInfo(..), mkSet, writeXMLTree, writeXMLTrees, Subject, SubjectMap, SubjectInfo(..),
	requireConsecutive, writeDoc, readXMLTrees, delWS) where

-- import ESP.Algebra
import Data.Bits
import Control.DeepSeq
import Control.Arrow
import Control.Applicative((<$>))
import Control.Monad
import GHC.Exts(build)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (|>), (><), (<|))
import Data.Foldable (toList)
-- import Data.Monoid
import Data.Maybe hiding (mapMaybe)
import Data.Either
import Data.IntMap hiding (map, toList)
-- import Text.XML.HXT.DOM.ShowXml
-- import Text.XML.HXT.DOM.TypeDefs
-- import Text.ParserCombinators.Parsec
import Debug.Trace
-- import Text.XML.HXT.Arrow.Pickle.Xml
import Text.XML.HXT.DOM.ShowXml (xshow)
import Text.XML.HXT.DOM.TypeDefs (XmlTree, XmlTrees)
import Text.XML.HXT.DOM.XmlKeywords
-- import Text.Parsec
import Text.ParserCombinators.Parsec
-- import qualified Text.ParserCombinators.Parsec as P
import Text.XML.HXT.Arrow.WriteDocument (writeDocumentToString)
-- import Text.XML.HXT.Arrow.ReadDocument 
import Text.XML.HXT.Parser.XmlParsec 
import Control.Arrow.ListArrow (LA (runLA))
import Text.XML.HXT.Arrow.Edit
-- import Text.XML.HXT.Arrow.XmlIOStateArrow
-- import System.IO.Unsafe

type Teacher = Int
type Room = Int
type Class = Int
type Time = Int
type Resource = Int
type Subject = Int
type IntSet = IntMap ()
type TeacherSet = IntSet
type RoomSet = IntSet
type ClassSet = IntSet
type TimeSet = IntSet
type ResourceSet = IntSet
type TeacherMap = IntMap
type RoomMap = IntMap
type ClassMap = IntMap
type TimeMap = IntMap
type ResourceMap = IntMap
type SubjectMap = IntMap
data RoomInfo = RInfo {rmId :: !Int, pos :: Maybe (Int, Int, Int), bId :: !Int} deriving (Show)
data TeacherInfo = TInfo {tId :: !Int, myTimes :: TimeSet} deriving (Show)
data ClassInfo = CInfo {cId :: !Int, classTeachers :: TeacherSet, validRooms :: RoomSet, classSize :: !Int,
				duration :: !Int, sections :: !Int, resourcesNeeded :: ResourceMap Int} deriving (Show)
data ResourceInfo = FloatingResource {rId :: !Int, resAvailable :: !Int, rentalCost :: !Int} deriving (Show)
data SubjectInfo = SInfo {sId :: !Int, subjectClasses :: ClassSet} deriving (Show)

instance NFData TeacherInfo where
	rnf = rnf . myTimes

instance NFData ClassInfo where
	rnf CInfo{..} = classTeachers `deepseq` validRooms `deepseq` rnf resourcesNeeded

instance NFData ResourceInfo

instance NFData RoomInfo where
	rnf = rnf . pos

instance NFData SubjectInfo where
	rnf SInfo{..} = rnf subjectClasses

mkSet :: [Int] -> IntSet
mkSet = fromList . map (, ())

writeXMLTree :: XmlTree -> String
writeXMLTree = writeXMLTrees . (:[])

writeXMLTrees :: XmlTrees -> String
writeXMLTrees = strReplace "</>" "" . strReplace "<//>" "" . xshow

readXMLTrees :: String -> Either ParseError XmlTrees
readXMLTrees str = case runParser (quoter >> getState) Seq.empty "quoter" (strReplace "\n" "" $ strReplace "\t" "" str) of
	Left pErr	-> Left pErr
	Right ans	-> parse content "XML parser" (toList ans)

delWS :: XmlTrees -> XmlTrees
delWS = map delWS' where
	delWS' = head . runLA removeAllWhiteSpace

writeDocArr :: LA XmlTree String
writeDocArr = writeDocumentToString []

writeDoc :: XmlTree -> String
writeDoc = head . runLA writeDocArr
{-
replaceBlank :: String -> String
replaceBlank inp = let Right ans = parse replacer "XML output" inp in ans
	where	replacer = (eof >> return "") <|> try (do
			string "</"
			optional (char '/')
			char '>'
			replacer) <|> 
		   do	xs <- manyTill anyChar (lookAhead (char '<'))
			liftM (xs ++) replacer-}

-- quoter :: GenParser Char (Seq Char) ()
quoter = eof <|> try (do
	char '='
	x <- many1 digit
	updateState (>< Seq.fromList ("=\"" ++ x ++ "\" "))
	quoter) <|> try (do
	char '\n' <|> char '\r'
-- 	updateState (>< Seq.fromList ("\\&"))
	quoter) <|> (do
	c <- anyChar
	updateState (|> c)
	quoter)
-- 	(do
-- 	c <- anyChar
-- 	cs <- manyTill anyChar (eof <|> (lookAhead (try (char '=') <|> try (anyChar >> char '&')) >> return ()))
-- 	updateState (>< (c <| Seq.fromList cs))
-- 	quoter)

strReplace :: Eq a => [a] -> [a] -> [a] -> [a]
strReplace find rep target = replacer target where
	replacer [] = []
	replacer (y:ys) = let
		rep' (x:xs) (z:zs)
			| x == z	= rep' xs zs
			| otherwise	= y:replacer ys
		rep' [] zs = rep ++ replacer zs
	  in rep' find (y:ys)

data IL a = C !Int a (IL a) | Nil

-- Takes O(n log k).
requireConsecutive :: Int -> IntMap () -> IntMap ()
requireConsecutive k m = reqCon k 1 0 mIL mIL
	where	mIL  = toIL m
		toIL = foldWithKey C Nil
		isectIL l10@(C k1 a1 l1) l20@(C k2 _ l2) = case compare k1 k2 of
			LT	-> isectIL l1 l20
			EQ	-> C k1 a1 (isectIL l1 l2)
			GT	-> isectIL l10 l2
		isectIL _ _ = Nil
		subMono :: Int -> IL a -> IL a
		subMono !i (C k v l) = C (k - i) v (subMono i l)
		subMono _ _ = Nil
		reqCon !k !x !s p m'
			| k == 0	= let	toL (C k a l) = (k, a):toL l
						toL _ = []
						in fromDistinctAscList (toL m')
			| testBit k 0	= reqCon (k `shiftR` 1) (x * 2) (s + x) p'
						(m' `isectIL` subMono s p)
			| otherwise	= reqCon (k `shiftR` 1) (x * 2) s p' m'
			where	p' = p `isectIL` subMono x p
		

mapKeysMonotonic :: (Int -> Int) -> IntMap a -> IntMap a
mapKeysMonotonic f m = fromDistinctAscList [(f k, v) | (k, v) <- assocs m]
