-- module Scheduling(Credential(..), Day(..), WeeklyAvailability, CrewMember(..), Schedule(..)) where
module Scheduling where
import Data.Set (Set, empty, insert, fromList)
import qualified Data.Set
import Data.Map (Map, empty, toList, fromList, lookup, adjust)
import Data.Maybe (isNothing, fromMaybe, listToMaybe, mapMaybe, catMaybes)
import Data.Foldable (find)
import Control.Applicative (Alternative(..))

data Credential = Supervisor | Trainer | CrewChief | Driver | Attendant | Observer
  deriving (Read, Show, Ord, Eq)
-- Additional types of credentials could be TrainerCrewChief | TrainerDriver

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Read, Show, Ord, Eq, Enum)

data CrewMember = Crew {name :: String, credentials :: Set Credential, availability :: Set Day}
  deriving (Show, Ord, Eq)

newMember :: String -> [Credential] -> [Day] -> CrewMember
newMember name' creds avail = Crew {name = name', credentials = Data.Set.fromList creds, availability = Data.Set.fromList avail}

-- It is possible to change Role into it's own data type to have the type system enforce the presence of two attendants / observers
data Role =  CrewChief_ | Driver_ | Attendant_
  deriving (Show)
-- type Assignment = (Role, CrewMember)
data Assignment = A {crew_chief :: Maybe CrewMember, driver :: Maybe CrewMember, thing1 :: Maybe CrewMember, thing2 :: Maybe CrewMember }
  deriving (Show)

-- | Schedule should hold the information for a week's potential schedule. You can expect relevant information like
-- | who is on duty, what days is everyone working on.
data Schedule = Schedule { crew :: Set CrewMember, daily_assignments :: Map Day Assignment }
  deriving (Show)

-- The goal regarding schedule generation is that Schedules are minimal data structures that carry no information by themselves
-- on wether it is a good or even a valid schedule. The plan being that users of the scheduler can create heuristics algorithms
-- who's sole goal is to grade how good a given schedule is and whether it is valid. This plug and play design is meant to allow
-- the user to fine tune the scheduler generating schedules that the user specifically wants.


data HeuristicResult g = HResult {isValid :: Bool, grade :: g}
  deriving (Eq, Show)

instance (Ord g) => Ord (HeuristicResult g) where
  -- compare :: Ord g => HeuristicResult g i -> HeuristicResult g i -> Ordering
  compare = undefined

-- | Heuristics plan to implement the Functor and Applicative constraints, as these constraints will allow for 
-- | the composition of Heuristics. Providing a method to create more complex constraints from simple ones. 
newtype Heuristic g = H (Schedule -> HeuristicResult g)

instance Functor Heuristic where
  -- fmap :: (a -> b) -> Heuristic a -> Heuristic b
  fmap f (H a) = H $ \s -> let (HResult v g) = a s
                           in HResult v (f g)

instance Applicative Heuristic where
  -- pure :: a -> Heuristic a
  pure a = H $ \_ -> HResult True a

  -- (<*>) :: Heuristic (a -> b) -> Heuristic a -> Heuristic b
  (<*>) (H fH) (H aH) = H $ \s -> let (HResult v1 f) = fH s
                                      (HResult v2 a) = aH s
                                  in HResult (v1 && v2) (f a)

instance Monad Heuristic where
  -- (>>=) :: Heuristic a -> (a -> Heuristic b) -> Heuristic b
  (>>=) (H aH) f = H $ \s -> let (HResult _ a) = aH s
                                 (H f') = f a
                                 (HResult v r) = f' s
                             in HResult v r


-- | An example heuristic might be to maximize the crew utilization. A schedule with more distinct crew memebers is more preferable to
-- | a schedule with less.
maximizeCrewUtilization :: Heuristic Int
maximizeCrewUtilization = H $ \schedule -> HResult { isValid = True, grade = length . crew $ schedule }


-- | rankSchedules takes in a Heuristic and a list of schedules producing a list of schedules
-- | ordered by the best grade, and returning all invalid schedules into the second tuple response
-- | All schedules returned will have the additional heuristic information added to them.
rankSchedules :: (Ord g) => Heuristic g -> [Schedule]  -> ([(Schedule, g)], [(Schedule, g)])
rankSchedules = undefined

-- | generateSchedules takes in a list of crew members returning every possible permutation of schedules from that set of crew memebers.
-- | generateSchedules should be lazily evaluated, and favor higher crew utilization for the start of the list.
-- | In the future generating permutations might become infeasible so looking into a strategies system
-- | where you would provide a strategy for generating schedules might be a good solution.
generateSchedules :: [CrewMember] -> [Schedule]
generateSchedules [] = [emptySchedule]
generateSchedules (member : xs) = do schedule <- generateSchedules xs
                                     genSchedulesFor schedule member

-- | getTopNSchedules takes in a set of crew members, a heuristic, a number N, and a number M, returning the top N valid schedules
-- | from the M generated schedules.
getTopNSchedules :: (Ord g) => [CrewMember] -> Heuristic g -> Int -> Int -> [(Schedule, g)]
getTopNSchedules crew heuristic n m = take n . fst . rankSchedules heuristic . take m $ generateSchedules crew


-- data Credential = Supervisor | Trainer | CrewChief | Driver | Attendant | Observer
-- data Role =  CrewChief_ | Driver_ | Attendant_

credentialToRole :: Credential -> Role
credentialToRole CrewChief = CrewChief_
credentialToRole Driver = Driver_
credentialToRole _ = Attendant_

genSchedulesFor :: Schedule -> CrewMember -> [Schedule]
genSchedulesFor s c = do day <- Data.Set.toList (availability c)
                         cred <- Data.Set.toList (credentials c)
                         let role = credentialToRole cred
                         return $ s `fromMaybe` addToSchedule s day role c 





emptyAssignment :: Assignment
emptyAssignment = A {crew_chief = Nothing, driver = Nothing, thing1 = Nothing, thing2 = Nothing }
emptySchedule :: Schedule
emptySchedule = Schedule { crew = Data.Set.empty, daily_assignments = Data.Map.fromList emptyAssignments}
  where emptyAssignments = map (\d -> (d, emptyAssignment)) (enumFrom $ toEnum 0)

-- [(day, [(role, crew_member)])]
getOpening :: Schedule -> Role -> [Day]
getOpening schedule CrewChief_ = fst <$> filter chiefOpening (toList $ daily_assignments schedule)
  where chiefOpening (_, A {crew_chief = chiefAssignment })
          | isNothing chiefAssignment = True
          | otherwise = False
getOpening schedule Driver_ = fst <$> filter driverOpening (toList $ daily_assignments schedule)
  where driverOpening (_, A {driver = driverAssignment })
          | isNothing driverAssignment = True
          | otherwise = False
getOpening schedule Attendant_ = fst <$> filter attendantOpening (toList $ daily_assignments schedule)
  where attendantOpening (_, A {thing1 = firstAssignment, thing2 = secondAssignment})
          | isNothing firstAssignment = True
          | isNothing secondAssignment = True
          | otherwise = False

opening :: Schedule -> Day -> Role -> Bool
opening (Schedule {daily_assignments = assignments}) day CrewChief_ = maybe True (isNothing . crew_chief) (Data.Map.lookup day assignments)
opening (Schedule {daily_assignments = assignments}) day Driver_    = maybe True (isNothing . driver) (Data.Map.lookup day assignments)
opening (Schedule {daily_assignments = assignments}) day Attendant_ = maybe True (\a -> isNothing (thing1 a) || isNothing (thing2 a)) (Data.Map.lookup day assignments)

isAvailableFor :: CrewMember -> Day -> Bool
isAvailableFor (Crew {availability = avail}) day = day `elem` avail

haveRelevantCredential :: CrewMember -> Role -> Bool
haveRelevantCredential (Crew {credentials = cred}) CrewChief_ = CrewChief `elem` cred
haveRelevantCredential (Crew {credentials = cred}) Driver_    = Driver `elem` cred
haveRelevantCredential (Crew {credentials = cred}) Attendant_ = Attendant `elem` cred

addToSchedule :: Schedule -> Day -> Role -> CrewMember -> Maybe Schedule
addToSchedule schedule@(Schedule {crew = existingCrew, daily_assignments = assignments}) day role crew
    |  isAvailableFor crew day
    && haveRelevantCredential crew role
    && opening schedule day role
    && notAlreadyAssigned schedule day crew =
      case role of
        CrewChief_ ->  Just $ schedule {crew = Data.Set.insert crew existingCrew, daily_assignments = adjust (\a -> a {crew_chief = Just crew}) day assignments}
        Driver_    ->  Just $ schedule {crew = Data.Set.insert crew existingCrew, daily_assignments = adjust (\a -> a {driver = Just crew}) day assignments}
        Attendant_ ->  Just $ schedule {crew = Data.Set.insert crew existingCrew, daily_assignments = adjust (firstOpening crew) day assignments}
    | otherwise = Nothing
  where firstOpening :: CrewMember -> Assignment -> Assignment
        firstOpening crew' assignment@(A {thing1 = Nothing}) = assignment {thing1 = Just crew'}
        firstOpening crew' assignment@(A {thing2 = Nothing}) = assignment {thing2 = Just crew'}
        firstOpening crew' assignment = assignment {thing1 = Just crew'}

elemAssignment :: CrewMember -> Assignment -> Bool
elemAssignment crew (A {crew_chief = c, driver = d, thing1 = a1, thing2 = a2}) = crew `elem` catMaybes [c, d, a1, a2]

-- | notAlreadyAssigned asserts that a crew member does not already exist in 
notAlreadyAssigned :: Schedule -> Day -> CrewMember -> Bool
notAlreadyAssigned (Schedule {daily_assignments = assignments}) day crew =
  maybe True (not . elemAssignment crew) (Data.Map.lookup day assignments)


greedyStrategy :: [CrewMember] -> Schedule
greedyStrategy crew = helper crew emptySchedule
  where helper :: [CrewMember] -> Schedule -> Schedule
        helper [] schedule = schedule
        helper (x : xs) schedule = let chiefOpening = getOpening schedule CrewChief_
                                       driverOpening = getOpening schedule Driver_
                                       attendantOpening = getOpening schedule Attendant_
                                       chiefSchedules = mapMaybe (\day -> addToSchedule schedule day CrewChief_ x) chiefOpening
                                       driverSchedules = mapMaybe (\day -> addToSchedule schedule day Driver_ x) driverOpening
                                       attendantSchedules = mapMaybe (\day -> addToSchedule schedule day Attendant_ x) attendantOpening
                                   in case (chiefSchedules, driverSchedules, attendantSchedules) of
                                       (newSchedule : _, _, _) -> helper (xs ++ [x]) newSchedule
                                       (_, newSchedule : _, _) -> helper (xs ++ [x]) newSchedule
                                       (_, _, newSchedule : _) -> helper (xs ++ [x]) newSchedule
                                       (_, _, _) -> schedule
