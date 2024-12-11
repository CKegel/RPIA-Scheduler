import Test.Hspec
import Scheduling hiding (driver)
import Data.Maybe
import Heuristic

main :: IO ()
main = hspec $ do 
  scheduleConstructionTests
  crewUtilizationTests
  fulfillmentTests
  compoundTests


scheduleConstructionTests = describe "addToSchedule" $ do
  it "Reject invalid day for given member" $ do
    isNothing $ addToSchedule emptySchedule Monday CrewChief_  crewChief
  it "Reject invalid role for given member" $ do
    isNothing $ addToSchedule emptySchedule Tuesday Attendant_ crewChief
  it "Accept valid day and role for given member" $ do
    isJust $ addToSchedule emptySchedule Tuesday Driver_ driver

crewUtilizationTests = describe "maximizeCrewUtilization" $ do
  it "Returns a grade of 0 for empty schedule" $ do
    0 == getGrade (runHeuristic maximizeCrewUtilization emptySchedule)
  it "Returns a grade of 2 for test schedule" $ do 
    2 == getGrade (runHeuristic maximizeCrewUtilization testSchedule)

fulfillmentTests = describe "maximizeFullday" $ do 
  it "Returns a grade of 0 for empty schedule" $ do
    0 == getGrade (runHeuristic maximizeFulldays emptySchedule)
  it "Returns a grade of 2 for test schedule" $ do 
    2 == getGrade (runHeuristic maximizeFulldays testSchedule)

compoundTests = describe "maximizeCrewUtilizationAndFulldays" $ do 
  it "Returns a grade of 0 for empty schedule" $ do
    0 == getGrade (runHeuristic maximizeCrewUtilizationAndFulldays emptySchedule)
  it "Returns a grade of 4 for empty schedule" $ do
    4 == getGrade (runHeuristic maximizeCrewUtilizationAndFulldays testSchedule)
    


crewChief = newMember "chief" [CrewChief] [Tuesday]
driver = newMember "driver" [Driver] [Tuesday]

getGrade :: HeuristicResult g -> g
getGrade HResult {grade = g} = g

-- | testSchedule is an example schedule for use in testing
testSchedule :: Schedule
testSchedule = s'
  where (Just s) = addToSchedule emptySchedule Tuesday CrewChief_ crewChief
        (Just s') = addToSchedule s Tuesday Driver_ driver
