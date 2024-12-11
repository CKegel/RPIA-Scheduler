module Heuristic where
import Scheduling(Schedule(..), CrewMember, generateSchedules, Assignment (..))
import Data.List (sortBy)
import Data.Map (elems)

data HeuristicResult g = HResult {isValid :: Bool, grade :: g}
  deriving (Eq, Show)

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


runHeuristic :: Heuristic g -> Schedule -> HeuristicResult g
runHeuristic (H f) = f

-- | rankSchedules takes in a Heuristic and a list of schedules producing a list of schedules
-- | ordered by the best grade, and returning all invalid schedules into the second tuple response
-- | All schedules returned will have the additional heuristic information added to them.
rankSchedules :: (Ord g) => Heuristic g -> [Schedule]  -> ([(Schedule, g)], [(Schedule, g)])
rankSchedules h schedules =  (sortByGrade valids, sortByGrade invalids)
  where results = map (runHeuristic h) schedules
        zippedResults = zip schedules results
        (valids, invalids) = foldr f ([], []) zippedResults
        f (schedule, HResult True g) (valids, invalids) = ((schedule, g) : valids, invalids)
        f (schedule, HResult False g) (valids, invalids) = (valids, (schedule, g) : invalids)
        sortByGrade = sortBy (\(_, a) (_, b) -> compare b a)


-- | getTopNSchedules takes in a set of crew members, a heuristic, a number N, and a number M, returning the top N valid schedules
-- | from the M generated schedules.
getTopNSchedules :: (Ord g) => [CrewMember] -> Heuristic g -> Int -> Int -> [(Schedule, g)]
getTopNSchedules crew heuristic n m = take n . fst . rankSchedules heuristic . take m $ generateSchedules crew


-- Example Heuristics

-- | An example heuristic might be to maximize the crew utilization. A schedule with more distinct crew memebers is more preferable to
-- | a schedule with less.
maximizeCrewUtilization :: Heuristic Int
maximizeCrewUtilization = H $ \schedule -> HResult { isValid = True, grade = length . crew $ schedule }

-- | maximizeFulldays grades schedules based on the maximum fulfillment of any given day.
-- | A schedule which has a day with all positions fulfilled will have a higher grade 
-- | than a schedule which has no day with all positions fulfilled.
maximizeFulldays :: Heuristic Int
maximizeFulldays = H $ \schedule -> HResult {isValid = True, grade = gradeF schedule}
  where assignments schedule = Data.Map.elems $ daily_assignments schedule
        fulfillment (A a b c d) = count a + count b + count c + count d
        count Nothing = 0
        count (Just _) = 1
        gradeF schedule = maximum $ map fulfillment $ assignments schedule


-- | This is an example of a composite heuristic. In here the grades from both crew utilization
-- | and fulfillment are summed together to produce the new composite grade.
maximizeCrewUtilizationAndFulldays :: Heuristic Int
maximizeCrewUtilizationAndFulldays = liftA2 (+) maximizeCrewUtilization maximizeFulldays
