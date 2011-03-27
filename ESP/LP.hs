{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module ESP.LP where
-- module ESP.LP (ESP(..), espProgram, readSol, espConstraints, usesRoomDefn, varTypes, teacherMovement) where

-- import Control.Parallel
-- import Data.Monoid
import Data.List(tails)
import Debug.Trace
-- import ESP.LP
import Data.LinearProgram hiding (varTypes)
import Control.Monad.LPMonad
-- import Data.LinearProgram.GLPK
import ESP.Common
import ESP.LP.Types
-- import Control.Applicative (liftA2)
import qualified Data.Map as M
import Control.DeepSeq
import Prelude hiding (lookup)	

readSol :: (a, M.Map ESPVar Double) -> [(Class, Room, Time)]
readSol (_, m) = [(c, r, t) | (ClassRoomTime c r t, v) <- M.assocs m, v > 0.5]

objectiveFunc' :: ESP -> ESPConstraints
objectiveFunc' = --liftA2 (*^) (size . allTeachers)
		 setObjective . (((5 :: Int) *^ (weightedStudentCapacity) ^-^ rentalCosts)) -- ^-^ teacherMovement)
-- 				^-^ teacherTravelFunc

objectiveFunc :: ESP -> ESPConstraints
objectiveFunc = --liftA2 (*^) (size . allTeachers)
		 setObjective . (((5 :: Int) *^ (totalStudentCapacity) ^-^ rentalCosts)) -- ^-^ teacherMovement)

-- | Given an ESP program, constructs a set of constraints.		 
espConstraints :: ESP -> ESPConstraints
espConstraints esp = mapM_ ($ esp) [resourceUsageBoundsDefn, absoluteResourceBounds,
	classPresentDefn, classicalTeacher , teacherAvailabilityConstraints , classPresentDefn,
	roomConstraints, classSections, oneClassPerRoom, respectPrerequisites, 
	fixedVariables, classHoursDefn, studentClassHoursDefn,
 	lowerResourceBounds, lunchConstraint {-, subjectsDistributed-}]
	-- classSections: Doesn't guarantee that we will schedule every section of every class,
	-- but bounds the number of sections of each class appropriately.

-- Given a, b, m, restricts the map m to the range [a..b].
-- mapSubsetInclusive :: Int -> Int -> IntMap a -> IntMap a
-- mapSubsetInclusive a b m = fst $ split (b+1) $ snd $ split (a-1) m
-- 
resUsage ::  ESP -> Time -> Resource -> ESPFunc
resUsage esp@ESP{..} tId resId = 
	gsum [req *^ ((classPres ! cId) ! tId) | (cId, req) <- assocs (resourceUsers ! resId)]
	where	resourceUsers = reverseMap (const resourcesNeeded) allClasses

-- Asserts that the resource availability numbers in the schedule specification are fixed upper bounds.
absoluteResourceBounds :: ESP -> ESPConstraints
absoluteResourceBounds ESP{..} =
	sequence_ [varEq --("Availability" ++ show rId) 
			(ResourceLimit rId) resAvailable
			| FloatingResource{..} <- elems allResources]

-- Defines the ResourceLimit variable as an upper bound on
-- usage of each resource at any given time.
resourceUsageBoundsDefn ::  ESP -> ESPConstraints
resourceUsageBoundsDefn esp@ESP{..} =
	sequence_ [leq --("Resource_Bounds_" ++ show resId ++ "_" ++ show tId)
			(resUsage esp tId resId)
			(var (ResourceLimit resId))
			| tId <- keys allBlocks, resId <- keys allResources]

-- Provides a lower bound on ResourceLimit.  Used when
-- some of this resource are stockpiled and won't need to be rented.
lowerResourceBounds :: ESP -> ESPConstraints
lowerResourceBounds ESP{..} =
	sequence_ [varGeq (ResourceLimit rId) resAvailable
			| FloatingResource{..} <- elems allResources]

-- objective function
-- Returns rental costs in dollars.
rentalCosts :: ESP -> ESPFunc
rentalCosts ESP{..} =
	gsum [rentalCost *^ var (ResourceLimit rId)
			| FloatingResource{..} <- elems allResources]

classHoursDefn ::  ESP -> ESPConstraints
classHoursDefn esp@ESP{..} = 
	equal {-"DefClassHours"-} (var ClassHours)
		(gsum [classPres ! cId ! tId | cId <- keys allClasses, tId <- keys allBlocks])

lunchConstraint :: ESP -> ESPConstraints
lunchConstraint esp@ESP{..} = mapM_ (\ lunch -> case lunch of
	[]	-> return ()
	_	-> sequence_ [
		leqTo (gsum [classPres ! cId ! tId | tId <- lunch])
			(length lunch - 1)
			| cId <- keys allClasses]) lunches

studentClassHoursDefn ::   ESP -> ESPConstraints
studentClassHoursDefn esp@ESP{..} = --traceShow (times, elems allClasses) $ 
	equal {-"DefSClassHours"-} (var StudentClassHours)
		(gsum [classSize *^ gsum [classPres ! cId ! tId | tId <- times] | CInfo{..} <- elems allClasses])
	where	times = keys allBlocks

-- objective function
-- returns sum, over all blocks, of the weight for that block, times the total student load for that block
weightedStudentCapacity ::  ESP -> ESPFunc
weightedStudentCapacity esp@ESP{..} = gsum 
	[weight *^ studentCapacityAtTime  tId esp | (tId, weight) <- assocs allBlocks]
	
totalStudentCapacity ::  ESP -> ESPFunc
totalStudentCapacity esp@ESP{..} = gsum
	[studentCapacityAtTime tId esp | tId <- keys allBlocks]

proportionate :: Int -> [(Int, ESPFunc)] -> ESPConstraints
proportionate tol wfs = sequence_
	[leqTo ((3 * w1) *^ f2 ^-^ (4 * w2) *^ f1) tol
			| (w1, f1) <- wfs, (w2, f2) <- wfs, f1 /= zero, f2 /= zero]

studentCapacityProportional ::  ESP -> ESPConstraints
studentCapacityProportional esp@ESP{..} =
	proportionate 300 [(wt, studentCapacityAtTime tId esp) | (tId, wt) <- assocs allBlocks]

studentCapacityAtTime ::  Time -> ESP -> ESPFunc
studentCapacityAtTime tId esp@ESP{..} = gsum [classSize *^ (classPres ! cId ! tId) | CInfo{..} <- elems allClasses]

-- Returns the number of teachers times the average number of different rooms used by each teacher
teacherMovement :: ESP -> ESPFunc
teacherMovement ESP{..} = varSum [UsesBuilding t bId | t <- keys allTeachers, bId <- keys $ mkSet (map bId (elems allRooms))]

fixedVariables :: ESP -> ESPConstraints
fixedVariables ESP{..} = sequence_
	[equalTo --("Fixed_" ++ show i) 
		(var (ClassRoomTime cId rId tId))
		(if y then 1 else 0)
		| (cId, rId, tId, y) <- fixed]

usesBuildingDefn :: ESP -> ESPConstraints
usesBuildingDefn ESP{..} = traceShow buildings $ 
	sequence_ [
		(var (UsesBuilding t bId)) `geq`
		(var (ClassRoomTime cId rId tId))
		| tId <- keys allBlocks, (t, myClasses) <- assocs teacherClasses, (bId, rs) <- assocs buildings,
			cId <- keys myClasses, rId <- keys rs] >>
	sequence_ [{-varBds (UsesBuilding tId bId) 0 1 >> -}setVarKind (UsesBuilding tId bId) BinVar
		| tId <- keys allTeachers, bId <- keys buildings]
	where	buildings = reverseMap (\ _ RInfo{..} -> singleton bId rmId) allRooms
		teacherClasses = reverseMap (const classTeachers) allClasses

-- usesRoomDefn :: ESP -> ESPConstraints
-- usesRoomDefn ESP{..} = sequence_
-- 	[sequence_ [leq' ("Room_Usage" ++ show t ++ "_" ++ show cId ++ "_" ++ show rId ++ "_" ++ show tId)
-- 			(var crt) -- | cId <- keys myClasses, tId <- keys allBlocks])
-- 			(var (UsesRoom t rId))
-- 			| crt@(ClassRoomTime cId rId tId) <- crts]
-- 		>> leq --("UsesRoom_UBound" ++ show t ++ "_" ++ show rId)
-- 			(var (UsesRoom t rId)) (varSum crts)
-- 			| (t, myClasses) <- assocs teacherClasses, rId <- keys allRooms,
-- 				let crts = [ClassRoomTime cId rId tId | cId <- keys myClasses, tId <- keys allBlocks]]
-- 	>> sequence_ [varBds (UsesRoom tId rId) 0 1 | tId <- keys allTeachers, rId <- keys allRooms]
-- 	where	teacherClasses = reverseMap (const classTeachers) allClasses
-- 
-- usesBothDefn :: ESP -> ESPConstraints
-- usesBothDefn ESP{..} = sequence_
-- 	[leq -- ("UsesBoth_L_" ++ show tId ++ "_" ++ show rId1 ++ "_" ++ show rId2)
-- 		 (var (UsesBoth tId rId1 rId2)) (var (UsesRoom tId rId1)) >>
-- 	 leq -- ("UsesBoth_R_" ++ show tId ++ "_" ++ show rId1 ++ "_" ++ show rId2)
-- 	 	(var (UsesBoth tId rId1 rId2)) (var (UsesRoom tId rId2)) >>
-- 	 leqTo -- ("UsesBoth_" ++ show tId ++ "_" ++ show rId1 ++ "_" ++ show rId2)
-- 	 	(var (UsesRoom tId rId1) ^+^ var (UsesRoom tId rId2) ^-^ var (UsesBoth tId rId1 rId2))
-- 			1
-- 		 >>
-- 	 varBds (UsesBoth tId rId1 rId2) 0 1
-- 	 	| let rms = keys allRooms, tId <- keys allTeachers, (rId1:rms') <- tails rms, rId2 <- rms']
{-
teacherTravelFunc :: ESP -> ESPFunc
teacherTravelFunc ESP{..} = gsum
	[dist p1 p2 *^ varSum [UsesBoth tId rId1 rId2 | tId <- teachers]
		| RInfo rId1 (Just p1):rms' <- tails rms, RInfo rId2 (Just p2) <- rms']
	where	teachers = keys allTeachers
		rms = elems allRooms
		dist (x1, y1, z1) (x2, y2, z2) = 
			abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)-}

-- Constrains the number of classes in any given room at once to 1. 
oneClassPerRoom :: ESP -> ESPConstraints
oneClassPerRoom ESP{..} = sequence_
	[atMostOneOf --("Room" ++ show rId ++ "_Time" ++ show tId ++ "_shorthand")
			-- A room is occupied at time t iff there is a class that started in that room at time t',
			-- where t' <= t < t + the duration of the class.
			-- Occupancy is a boolean: there may be no more than 1 class in a given room.
			-- (The integral constraint is unnecessary, as it is a sum of booleans.)
			[ClassRoomTime cId rId tId'
					| CInfo{cId, duration} <- elems allClasses, let d = duration,
						tId' <- keys $ mapSubsetInclusive (tId-d+1) tId allBlocks]
				| rId <- keys allRooms, tId <- keys allBlocks]

-- A class is in session iff it started in the past <class-duration> blocks.
-- Sincce this is a 0-1 variable, has the side effect of forbidding a class 
-- from starting more than once per duration.
classPresentDefn :: ESP -> ESPConstraints
classPresentDefn ESP{..} = sequence_ [equal --("Class" ++ show cId ++ "_Present" ++ show tId ++ "_shorthand")
				(varSum [ClassRoomTime cId rId tId' | tId' <- keys $ mapSubsetInclusive (tId-d+1) tId allBlocks,
										rId <- keys allRooms])
				(var (ClassPresent cId tId))
				>> varBds (ClassPresent cId tId) 0 1
				| CInfo{..} <- elems allClasses, let d = duration, tId <- keys allBlocks]

-- classPresent :: ESP -> Class -> Time -> ESPFunc
-- classPresent ESP{..} cId tId = varSum [ClassRoomTime cId rId tId' | tId' <- keys $ mapSubsetInclusive (tId-d+1) tId allBlocks,
-- 										rId <- keys allRooms]
-- 		where	CInfo{duration = d} = allClasses ! cId

classSections :: ESP -> ESPConstraints
classSections ESP{..} = 
	sequence_ [do	leqTo --("Class" ++ show cId ++ "_Num_Sections") 
			-- A class may start no more times than it has sessions.
				(varSum [ClassRoomTime cId rId tId | rId <- keys allRooms, tId <- keys allBlocks])
				sections
			sequence_ [equalTo (classPres ! cId ! tId) 0 | tId <- [maxTime - duration + 2..maxTime]]
				| CInfo{..} <- elems allClasses]
	where	maxTime = fst (findMax allBlocks)

-- Asserts that we will schedule every section of every class.
-- perfectSchedule :: ESP -> ESPConstraints
-- perfectSchedule ESP{..} = 
-- 	sequence_ [equalTo --("Class" ++ show cId ++ "_Num_Sections") 
-- 			-- A class may start no more times than it has sessions.
-- 				(varSum [ClassRoomTime cId rId tId | rId <- keys allRooms, tId <- keys allBlocks])
-- 				sections
-- 				| CInfo{..} <- elems allClasses]

nearPerfect :: Int -> ESP -> ESPConstraints
nearPerfect n esp@ESP{..} = classSections esp >> 
	geqTo (varSum [ClassRoomTime cId rId tId | cId <- keys allClasses, rId <- keys allRooms, tId <- keys allBlocks])
		(sum [sections | CInfo{..} <- elems allClasses] - n)

-- Constrains classes to be in valid rooms.
roomConstraints :: ESP -> ESPConstraints
roomConstraints ESP{..} =
	noneOf --"Invalid_Rooms" 
		[ClassRoomTime cId rId tId | CInfo{cId, validRooms} <- elems allClasses, rId <- keys (allRooms `difference` validRooms),
							tId <- keys allBlocks]

-- Constrains teachers to be classical -- that is, not in a superposition of several rooms at once.  ^_^
classicalTeacher ::  ESP -> ESPConstraints
classicalTeacher esp@ESP{..} =
	sequence_ [leqTo --("Teacher" ++ show tId ++ "_In_One_Place_At_One_Time" ++ show timeId)
		-- Teachers cannot be teaching more than one class at once.
			(gsum (map (! timeId) myClasses')) 1
			| timeId <- keys allBlocks, myClasses <- elems teacherClasses, let myClasses' = elems (intersection classPres myClasses)]
	where	teacherClasses = reverseMap (const classTeachers) allClasses

-- Classes can only be in session when all of their teachers are available.
teacherAvailabilityConstraints ::  ESP -> ESPConstraints
teacherAvailabilityConstraints esp@ESP{..} =
	sequence_ [equalTo (classPres ! cId ! tId) 0 | (TInfo{..}, myClasses) <- elems (intersectionWith (,) allTeachers teacherClasses),
			tId <- keys (allBlocks `difference` myTimes), cId <- keys myClasses]
	where	teacherClasses = reverseMap (const classTeachers) allClasses
-- 	sequence_ [equalTo (classPres ! cId ! tId) 0 | (cId, myTimes) <- classTimes, tId <- keys (foldr (\ k -> insert k 0)
-- 			(allBlocks `difference` myTimes) [(maxTime - duration (allClasses ! cId) + 2)..maxTime])]
-- 	where	classTimes = assocs $ fmap classTime allClasses where
-- 			classTime CInfo{classTeachers, duration} = 
-- 				foldr (intersection . myTimes) (const () <$> allBlocks) $
-- 					elems (intersection allTeachers classTeachers)
-- 		maxTime = fst (findMax allBlocks)

-- Restricts classes to occur after at least one section of each of their prerequisites has ended.
respectPrerequisites ::  ESP -> ESPConstraints
respectPrerequisites esp@ESP{..} = sequence_
	[leq	-- cId1 may only start at time t' if a prerequisite class has ended before t'.
			(classPres ! cId1 ! t') hasPrereq
			| (cId0, cId1) <- prereqs, Just CInfo{duration = duration0} <- 
				return (lookup cId0 allClasses),
				t <- keys allBlocks, 
				let hasPrereq = gsum [classPres ! cId0 ! tId | tId <- keys $ fst (split (t - duration0 + 1) allBlocks)],
				-- hasPrereq is an expression that is positive iff any instances of the prerequisite class have ended yet.
				t' <- keys $ snd (split (t-1) allBlocks)]
				
reverseMap :: (Int -> b -> IntMap c) -> IntMap b -> IntMap (IntMap c)
reverseMap image = foldWithKey (\ k v -> unionWith union (fmap (singleton k) (image k v))) empty

totalClassHours ::  ESP -> ESPFunc
totalClassHours esp@ESP{..} = gsum [classPres ! cId ! tId | tId <- keys allBlocks, cId <- keys allClasses]

totalClassSections :: ESP -> ESPFunc
totalClassSections ESP{..} = varSum [ClassRoomTime cId rId tId | tId <- keys allBlocks, rId <- keys allRooms, cId <- keys allClasses]

-- Mandates that each set of classes in a Subject be evenly distributed.
subjectsDistributed ::  ESP -> ESPConstraints
subjectsDistributed esp@ESP{..} = 
	sequence_ 
		[proportionate 40 [(wt, sD) | (sD, (_, wt)) <- subjDensities (intersection allClasses subjectClasses)]
			| SInfo{..} <- elems allSubjects]
	where	subjDensities cs = [(gsum [classSize *^ classPres ! cId ! t | CInfo{..} <- elems cs], (t, wt))
					| (t, wt) <- assocs allBlocks]
		allWeights = sum (elems allBlocks)

-- -- distinctClasses :: ESP -> ESPFunc
-- distinctClasses ESP{..} = varSum [ClassOccurs cId | cId <- keys allClasses]

varTypes :: ESP -> ESPConstraints
varTypes ESP{..} = sequence_ [ClassRoomTime c r t `setVarKind` BinVar | c <- classes, r <- rooms, t <- blocks]
	where	classes = keys allClasses
		rooms = keys allRooms
		blocks = keys allBlocks
	-- resource usage variables are the sums of boolean variables, and are therefore guaranteed integers,
	-- which is perfectly fine

espProgram :: ESP -> LP ESPVar Int
espProgram esp = esp `deepseq` trace "Preprocessing complete" $ {-traceShow (M.lookup ClassHours (varBounds ans))-} ans
	where	ans = execLPM $ do
			setDirection Max
			objectiveFunc esp 
			espConstraints esp
			varTypes esp

espProgram' :: ESP -> LP ESPVar Int
espProgram' esp = esp `deepseq` trace "Preprocessing complete" $ ans --traceShow (sum (map sections (elems (allClasses esp)))){-traceShow (M.lookup ClassHours (varBounds ans))-} ans
	where	ans = execLPM $ do
			setDirection Max
			objectiveFunc' esp 
			espConstraints esp
			varTypes esp
