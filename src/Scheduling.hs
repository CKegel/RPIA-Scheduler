module Scheduling(Credential(..), Day(..), WeeklyAvailability, CrewMember(..), Schedule(..)) where 
import Data.Set (Set)
import Data.Map (Map)

data Credential = Supervisor | Trainer | CrewChief | Driver | Attendant | Observer 
-- Additional types of credentials could be TrainerCrewChief | TrainerDriver

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

-- | Internal weekly availability structure, should interacted with special helper functions
data WeeklyAvailability = Availability {monday :: Bool, tuesday :: Bool, wednesday :: Bool, thursday :: Bool, friday :: Bool, saturday :: Bool, sunday :: Bool}

emptyAvailability :: WeeklyAvailability
emptyAvailability = Availability {monday = False, tuesday = False, wednesday = False, thursday = False, friday = False, saturday = False, sunday = False}

getAvailabilityFromDays :: [Day] -> WeeklyAvailability
getAvailabilityFromDays = foldr (\day acc -> case day of 
  Monday -> acc {monday = True}
  Tuesday -> acc {tuesday = True}
  Wednesday -> acc {wednesday = True}
  Thursday -> acc {thursday = True}
  Friday -> acc {friday = True}
  Saturday -> acc {saturday = True}
  Sunday -> acc {sunday = True}) emptyAvailability

data CrewMember = Crew {name :: String, credentials :: Set Credential, availability :: WeeklyAvailability}

-- It is possible to change Role into it's own data type to have the type system enforce the presence of two attendants / observers
type Role = Credential
type Assignment = (Role, CrewMember)

-- | Schedule should hold the information for a week's potential schedule. You can expect relevant information like
-- | who is on duty, what days is everyone working on.
data Schedule = Schedule { crew :: [CrewMember], daily_assignments :: Map Day [Assignment] }

-- The goal regarding schedule generation is that Schedules are minimal data structures that carry no information by themselves
-- on wether it is a good or even a valid schedule. The plan being that users of the scheduler can create heuristics algorithms
-- who's sole goal is to grade how good a given schedule is and whether it is valid. This plug and play design is meant to allow
-- the user to fine tune the scheduler generating schedules that the user specifically wants.


data HeuristicResult g i = HResult {isValid :: Bool, grade :: g, additional_info :: i}
  deriving (Eq, Show)

instance (Ord g, Eq i) => Ord (HeuristicResult g i) where 
  -- compare :: Ord g => HeuristicResult g i -> HeuristicResult g i -> Ordering
  compare = undefined

-- | Heuristics plan to implement the Functor and Applicative constraints, as these constraints will allow for 
-- | the composition of Heuristics. Providing a method to create more complex constraints from simple ones. 
newtype Heuristic g i = H (Schedule -> (HeuristicResult g i))


-- | rankSchedules takes in a Heuristic and a list of schedules producing a list of schedules
-- | ordered by the best grade, and returning all invalid schedules into the second tuple response
-- | All schedules returned will have the additional heuristic information added to them.
rankSchedules :: (Ord g, Eq i) => Heuristic g i -> [Schedule]  -> ([(Schedule, g, i)], [(Schedule, g, i)])
rankSchedules = undefined


-- | generateSchedules takes in a list of crew members returning every possible permutation of schedules from that set of crew memebers.
-- | generateSchedules should be lazily evaluated, and favor higher crew utilization for the start of the list.
-- | In the future generating permutations might become infeasible so looking into a strategies system
-- | where you would provide a strategy for generating schedules might be a good solution.
generateSchedules :: [CrewMember] -> [Schedule]
generateSchedules = undefined


-- | getTopNSchedules takes in a set of crew members, a heuristic, a number N, and a number M, returning the top N valid schedules
-- | from the M generated schedules.
getTopNSchedules :: (Ord g, Eq i) => [CrewMember] -> Heuristic g i -> Int -> Int -> [(Schedule, g, i)]
getTopNSchedules crew heuristic n m = take n . fst . rankSchedules heuristic . take m $ generateSchedules crew


-- | An example heuristic might be to maximize the crew utilization. A schedule with more distinct crew memebers is more preferable to
-- | a schedule with less.
maximizeCrewUtilization :: Heuristic Int ()
maximizeCrewUtilization = undefined
