{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module ESP.XMLParser where

import Data.Maybe
import Debug.Trace
import ESP.Common
import Data.Ord
import qualified ESP.Common as C
import qualified Data.Map as M
import qualified Data.List as List
import Text.XML.HXT.Arrow.Pickle
-- import Text.XML.HXT.DOM.ShowXml
-- import Text.XML.HXT.Parser.XmlParsec
import ESP.LP.Types (ESP, esp)
import Prelude hiding (lookup)

data ESPTag = BT {unBT :: !BlockTag} | ResT {unResT :: !ResTag} | RmT {unRmT :: !RoomTag} | TT {unTT :: !TeacherTag} | CT {unCT :: !ClassTag}
	| ST {unST :: !SubjectTag} deriving (Show)
data BlockTag = BlockTag {blockId :: !Int, blockName :: Maybe String, blockWeight :: Int, isLunch :: Maybe Int} deriving (Show)
data ResTag = FixedRes {resId :: !Int, resName :: Maybe String} |
		FloatRes {resId :: !Int, resName :: Maybe String, quantity :: !Int, rentCost :: !Int} deriving (Show)
data RoomTag = RoomTag {roomId :: !Int, roomName :: Maybe String, capacity :: !Int, resources :: [(Int, Int)], pos :: Maybe (Int, Int, Int),
				building :: !Int} deriving (Show)
data ClassTag = ClassTag {cId :: !Int, regId :: Maybe String, cName :: Maybe String, cCap :: !Int, nSections :: !Int, subj :: Maybe String,
				duration :: !Int, resReqs :: [(Int, Int)], teachers :: [Int], prereqs :: [Int],
				fixedPieces :: [(Room, Time)]} deriving (Show)
data TeacherTag = TeacherTag {tId :: !Int, tName :: Maybe String, availability :: [Int]} deriving (Show)
data SubjectTag = SubjectTag {sId :: !Int, sName :: Maybe String, classes :: [Int]} deriving (Show)

processDocument :: String -> (ESP, ClassMap String, RoomMap String, TimeMap String)
processDocument input = case fmap delWS (readXMLTrees input) of
	Left pErr	-> error (show pErr)
	Right tags0	-> -- trace (writeXMLTrees tags0) $ 
		let tags = [tag | xTree <- tags0, esptags <- mLift (unpickleDoc (xpList tagPickler) xTree), tag <- esptags] in
			length tags `seq` trace "Desugaring complete" (tagsToESP tags)
	where	

mLift :: MonadPlus m => Maybe a -> m a
mLift = maybe mzero return

tagsToESP :: [ESPTag] -> (ESP, ClassMap String, RoomMap String, TimeMap String)
tagsToESP tags = (esp (fromList [(C.tId t, t) | t <- allTeachers]) 
			(fromList [(C.cId c, c) | c <- allClasses])
			(fromList [(roomId, RInfo roomId pos building) | RmT RoomTag{..} <- tags])
			(fromList [(blockId, blockWeight) | BT BlockTag{..} <- tags])
			(fromList [(rId r, r) | r <- floatRes])
			(fromList [(i, SInfo i (mkSet classes)) | (i, classes) <- zip [1..] (M.elems subjectMap)])
			[(cId, req) | CT ClassTag{..} <- tags, req <- prereqs]
			(M.elems lunches)
			[(cId, r, t, True) | CT ClassTag{..} <- tags, (r,t) <- fixedPieces],
			fromList [(cId, fromMaybe (show cId) regId) | CT ClassTag{..} <- tags],
			fromList [(roomId, fromMaybe (show roomId) roomName) | RmT RoomTag{..} <- tags],
			fromList [(blockId, fromMaybe (show blockId) blockName) | BT BlockTag{..} <- tags])
	where	-- allBlocks = [blockId | BT BlockTag{..} <- tags]
		subjectMap :: M.Map String [Class]
		subjectMap = M.fromListWith (++)
			[(subj, [cId]) | CT ClassTag{subj = Just subj, cId} <- tags]
		lunches = M.fromListWith (++)
			[(lId, [blockId]) | BT BlockTag{isLunch = Just lId, blockId} <- tags]
		floatRes :: [ResourceInfo]
		floatRes = [FloatingResource resId quantity rentCost | ResT FloatRes{..} <- tags]
		fixed = fromList [(resId, ()) | ResT FixedRes{..} <- tags]
		allTeachers = [TInfo tId (mkSet availability) | TT TeacherTag{..} <- tags]
		allClasses = [let myRooms = foldr intersection (roomsWithCapacity cCap `intersection` roomsAtMost cCap)
					[roomsWithResource res qty | (res, qty) <- resReqs, res `member` fixed]
				in CInfo cId (mkSet teachers) myRooms cCap duration nSections
					(fromList [(res, qty) | (res, qty) <- resReqs, res `notMember` fixed])
					| CT ClassTag{..} <- tags]
		allRooms = const () <$> roomMap
		roomMap = fromList [(roomId rt, rt) | RmT rt <- tags]
		roomsWithCapacity :: Int -> IntMap ()
		roomsWithCapacity cap = foldWithKey (const union) empty $ snd $ split (cap - 1) roomsWithCapacityMap
		roomsWithCapacityMap = reverseMap (\ _ rt -> singleton (capacity rt) ()) roomMap
		roomsAtMost :: Int -> IntMap ()
		roomsAtMost cap = foldWithKey (const union) empty $ fst $ split (5 * cap `quot` 4) roomsWithCapacityMap
		roomsWithResource :: Int -> Int -> RoomSet
		roomsWithResource res qty = maybe empty (mkSet . map snd . dropWhile ((< qty) . snd)) (lookup res roomsWithResourceMap)
		roomsWithResourceMap :: ResourceMap [(Int, Room)]
		roomsWithResourceMap = fmap (List.sortBy (comparing fst)) $ fromListWith (++)
			[(res, [(qty, roomId)]) | RoomTag{roomId, resources} <- elems roomMap, (res, qty) <- resources]
-- 		roomsWithResourceMap = reverseMap (const (`singleton` ())) <$> reverseMap (const (fromList . resources)) roomMap

reverseMap :: (Int -> b -> IntMap c) -> IntMap b -> IntMap (IntMap c)
reverseMap image = foldWithKey (\ k v -> unionWith union (fmap (singleton k) (image k v))) empty

tagPickler :: PU ESPTag
tagPickler = xpAlt ix [xpWrap (BT, unBT) blockPickler, xpWrap (ResT, unResT) resPickler, xpWrap (RmT, unRmT) roomPickler, 
		xpWrap (TT, unTT) teacherPickler, xpWrap (CT, unCT) classPickler, xpWrap (ST, unST) subjectPickler] where
	ix BT{} = 0
	ix ResT{} = 1
	ix RmT{} = 2
	ix TT{} = 3
	ix CT{} = 4
	ix ST{} = 5

blockPickler :: PU BlockTag
blockPickler = xpWrap (toTag, fromTag)
		(xpElem "block" $ xp4Tuple (xpAttr "id" xpickle) (xpAttrImplied "name" xpText) (xpAttrImplied "weight" xpickle)
					(xpAttrImplied "lunch" xpickle))
	where	toTag (i, nam, wt, lunch) = let l = lunch in
			BlockTag i nam (fromMaybe 1 wt) l
		fromTag (BlockTag i nam wt lunch) = (i, nam, Just wt, lunch)

subjectPickler :: PU SubjectTag
subjectPickler = xpElem "subject" $ xpWrap (toSubj, fromSubj) $
	xpTriple (xpAttr "id" xpickle) (xpAttrImplied "name" xpText)
		(xpList (xpElem "class" (xpAttr "id" xpickle)))
	where	toSubj (i, nam, cs) = SubjectTag i nam cs
		fromSubj (SubjectTag i nam cs) = (i, nam, cs)

resPickler :: PU ResTag
resPickler = xpElem "resource" $
	(xpWrap (toRes, fromRes))
		(xp5Tuple (xpAttrImplied "fixed" xpickle)
			(xpAttr "id" xpickle)
			(xpAttrImplied "name" xpText)
			(xpAttrImplied "avail" xpickle)
			(xpAttrImplied "rentcost" xpickle))
	where	toRes :: (Maybe Int, Int, Maybe String, Maybe Int, Maybe Int) -> ResTag
		toRes (Just 1, rId, nam, _, _)
			= FixedRes rId ( nam)
		toRes (_, rId, nam, avail, rcost)
			= FloatRes rId ( nam) (fromMaybe 1 avail) (fromMaybe 1 rcost)
		fromRes (FixedRes rId rNam)
			= (Just 1, rId, rNam, Nothing, Nothing)
		fromRes (FloatRes rId rNam avail rcost)
			= (Just 0, rId, rNam, Just avail, Just rcost)

roomPickler :: PU RoomTag
roomPickler = xpElem "room" $ xpWrap (toRoom, fromRoom) $
	(xp6Tuple (xpAttr "id" xpickle) (xpAttrImplied "name" xpText)
		(xpAttr "capacity" xpickle) (xpList $
			(xpElem "has-resource" $
				xpWrap (second (fromMaybe 1), second Just) $
					xpPair (xpAttr "id" xpickle) (xpAttrImplied "quantity" xpickle)))
		(xpTriple (xpAttrImplied "x" xpickle)
			  (xpAttrImplied "y" xpickle)
			  (xpAttrImplied "z" xpickle))
		(xpAttrImplied "building" xpickle))
	where	toRoom (rId, rN, rCap, rRes, (x, y, z), b) = RoomTag rId rN rCap rRes (liftM3 (,,) x y z) (fromMaybe (rId) b)
		fromRoom (RoomTag rId rN rCap rRes Nothing b) = (rId, rN, rCap, rRes, (Nothing, Nothing, Nothing), Just b)
		fromRoom (RoomTag rId rN rCap rRes (Just (x,y,z)) b) = (rId, rN, rCap, rRes, (Just x, Just y, Just z), Just b)
		
teacherPickler :: PU TeacherTag
teacherPickler = xpWrap (toTeacher, fromTeacher) $ xpElem "teacher" $ 
	(xpTriple (xpAttr "id" xpickle) (xpAttrImplied "name" xpText)
		(xpList $ xpElem "available" $ xpAttr "block" xpickle))
	where	toTeacher (tId, tN, tAv) = {- traceShow tAv $ -} TeacherTag tId tN tAv
		fromTeacher (TeacherTag tId tN tAv) = (tId, tN, tAv)

-- xpEList :: PU a -> PU b -> PU ([a], [b])
-- xpEList pa pb = xpWrap (toEList, fromEList) (xpList (xpEither pa pb))

classPickler :: PU ClassTag
classPickler =  xpWrap (toClass, fromClass) $ xpElem "class" $
	xpTriple (xp4Tuple (xpAttr "id" xpickle) (xpAttrImplied "regID" xpText) (xpAttrImplied "name" xpText)
			(xpAttr "cap" xpickle))
		(xpTriple (xpAttr "sections" xpickle) (xpAttrImplied "category" xpText) (xpAttr "duration" xpickle))
		(xpList (xpEither (xpEither ((xpElem "needs-resource" $ xpPair (xpAttr "id" xpickle) (xpAttr "qty" xpickle)))
				(xpElem "section" $ xpPair (xpAttr "room" xpickle) (xpAttr "time" xpickle)))
			(xpEither ((xpElem "prereq" $ xpAttr "pid" xpickle))
				(xpElem "class-teacher" (xpAttr "id" xpickle)))))
-- 		(xpList (xpEither 
-- 				(xpEither (xpElem "needsres" $ xpPair (xpAttr "rid" xpickle) (xpAttr "qty" xpickle))
-- 					(xpElem "section" $ xpPair (xpAttr "room" xpickle) (xpAttr "time" xpickle)))
-- 				(xpEither (xpElem "prereq" $ xpAttr "pid" xpickle) (xpElem "cteacher" (xpAttr "cid" xpickle)))))
	where	xpEither pa pb = -- xpAlt (either (const 0) (const 1))
-- 			[xpWrap (Left, (\ (Left x) -> x)) pa, xpWrap (Right,  (\ (Right x) -> x)) pb]
			xpCondSeq (xpWrap (Left, \ (Left x) -> x) pa) id 
				(xpWrap (Right, \ (Right x) -> x) pb) xpLift
		toClass ((cId, regId, cName, cCap), (nSec, subj, dur), reqs) -- (resReqs, fixes, prereqs, tchrs))
			= let (reqs1, reqs') = partitionEithers reqs
			      (resReqs, fixes) = partitionEithers reqs1
			      (prereqs, tchrs) = partitionEithers reqs' in --traceShow l $ 
				ClassTag cId regId cName cCap nSec subj dur resReqs tchrs prereqs fixes 
		fromClass (ClassTag cId regId cName cCap nSec subj dur resReqs tchrs prereqs fixes) =
			((cId, regId, cName, cCap), (nSec, subj, dur), (map (Left . Left) resReqs ++ map (Left . Right) fixes 
					++ map (Right . Left) prereqs ++ map (Right . Right) tchrs)) --[((resReqs, fixes, prereqs, tchrs))
					{- [Left (Left res) | res <- resReqs] ++ [Right (Left prereq) | prereq <- prereqs]
				-- ++ [Right (Right tchr) | tchr <- tchrs] ++ [Left (Right fx) | fx <- fixes]) -}
