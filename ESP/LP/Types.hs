{-# LANGUAGE RecordWildCards #-}

module ESP.LP.Types where

import Control.DeepSeq
import ESP.Common

import Data.LinearProgram
import Control.Monad.LPMonad

import qualified Data.Map as M

type ESPFunc = LinFunc ESPVar Int
type ESPConstraints = LPM ESPVar Int ()
data ESPVar = ClassRoomTime !Class !Room !Time |
		-- a Boolean variable declaring that this class
		-- starts in this room at this time
	RoomOccupied !Room !Time | ClassOccurs !Class | --UsesBoth !Teacher !Room !Room |
	ClassPresent !Class !Time | TeacherOccupied !Teacher !Time | ResourceLimit !Resource |
	ResourceUsage !Time !Resource | UsesBuilding !Teacher !Int |
	TransitionsBuilding !Teacher !Int !Int !Time |
	StudentClassHours | ClassHours deriving (Eq, Ord, Show)

-- instance (Ord v, NFData v, NFData c) => NFData (Constraint v c) where
-- 	rnf (Constr lab m b) = lab `deepseq` m `deepseq` b `seq` ()

-- instance NFData ESPVar

-- instance (NFData k, NFData a) => NFData (M.Map k a) where
-- 	rnf = rnf . M.assocs

data ESP = ESP
	{allTeachers :: TeacherMap TeacherInfo,
	 allClasses :: ClassMap ClassInfo,
	 allRooms :: RoomMap RoomInfo,
	 allBlocks :: TimeMap Int, -- weight for each block
	 allResources :: ResourceMap ResourceInfo,
	 allSubjects :: SubjectMap SubjectInfo,
	 prereqs :: [(Class, Class)],
	 fixed :: [(Class, Room, Time, Bool)],
	 lunches :: [[Time]],
	 classPres :: ClassMap (TimeMap ESPFunc)} deriving (Show)

instance NFData ESP where
	rnf ESP{..} = allTeachers `deepseq` allClasses `deepseq` allRooms `deepseq` allBlocks `deepseq` allResources `deepseq` 
		allSubjects `deepseq` rnf prereqs

fixVars :: [ESPVar] -> Int -> ESPConstraints
fixVars vs x = sequence_ [varEq v x | v <- vs]

atMostOneOf :: [ESPVar] -> ESPConstraints
atMostOneOf vars = leqTo (varSum vars) 1

noneOf :: [ESPVar] -> ESPConstraints
noneOf vs = fixVars vs 0

esp :: TeacherMap TeacherInfo -> ClassMap ClassInfo -> RoomMap RoomInfo -> TimeMap Int
	-> ResourceMap ResourceInfo -> SubjectMap SubjectInfo -> [(Class, Class)] -> [[Time]] -> [(Class, Room, Time, Bool)] -> ESP
esp allTeachers allClasses allRooms allBlocks allResources allSubjects prereqs lunches fixed =
	ESP allTeachers allClasses allRooms allBlocks allResources allSubjects prereqs fixed lunches $ fmap (\ CInfo{duration=d, ..} -> 
		mapWithKey ( \ tId _ -> var $ ClassPresent cId tId {-varSum [ClassRoomTime cId rId tId' | tId' <- keys $ mapSubsetInclusive (tId-d+1) tId allBlocks,
										rId <- keys allRooms]-}) allBlocks) allClasses

mapSubsetInclusive :: Int -> Int -> IntMap a -> IntMap a
mapSubsetInclusive a b m = fst $ split (b+1) $ snd $ split (a-1) m