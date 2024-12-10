{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Lib
import System.IO
import Scheduling (CrewMember(..), Credential(..), Day(..), newMember, greedyStrategy, Schedule, emptySchedule, generateSchedules)
import Persistence
import Heuristic
import Display
import Brick.Widgets.Table
import Brick
import Control.Monad.State


getDays :: [Day] -> IO [Day]
getDays days = do putStrLn "Input day available or \"q\" to stop."
                  result <- getLine
                  if result == "q" then 
                    return days
                  else getDays $ (read result) : days


getCredentials :: [Credential] -> IO [Credential]
getCredentials cred = do putStrLn "Input credential or \"q\" to stop."
                         result <- getLine
                         if result == "q" then 
                           return cred 
                         else getCredentials $ (read result) : cred
            

getMember :: IO CrewMember
getMember = do putStrLn "Registering a crew member."
               putStr "Name: "
               hFlush stdout
               name <- getLine
               credentials <- getCredentials []
               days <- getDays []
               return $ newMember name credentials days

getMembers :: [CrewMember] -> IO [CrewMember]
getMembers crew = do putStrLn "Would you like to register a new crew member? y/n"
                     reply <- getLine
                     if reply == "y" || reply == "Y" then 
                       do member <- getMember
                          getMembers (member : crew)
                     else return crew


getInt :: IO (Int)
getInt = readLn

data MenuChoice = Display
                | LoadCrew
                | AddCrew
                | SaveCrew
                | ClearCrew
                | ListCrew
                | GenerateSchedules
                | NextSchedule
                | Quit
  deriving(Show, Bounded, Enum)

printOptions :: IO ()
printOptions = do putStrLn "0: Display top schedule"
                  putStrLn "1: Load crew from file"
                  putStrLn "2: Add crew manually"
                  putStrLn "3: Write crew to file"
                  putStrLn "4: Clear crew"
                  putStrLn "5: List crew"
                  putStrLn "6: Generate schedules"
                  putStrLn "7: Get next schedule"
                  putStrLn "8: Quit"


getUserChoice :: forall a. (Bounded a, Enum a) => IO (a)
getUserChoice = do 
  let lower = fromEnum (minBound :: a)
  let upper = fromEnum (maxBound :: a)
  putStrLn "Enter Choice: "
  choice <- getInt
  if lower <= choice && choice <= upper then
    return $ toEnum choice
  else do
    putStrLn $ "Incorrect Choice: Options are [" ++ show lower ++ ", " ++ show upper ++ "]"
    getUserChoice


data MainState = S {quit :: Bool, schedules :: [Schedule], crew :: [CrewMember]}

initState :: MainState
initState = S {quit = False, schedules = [emptySchedule], crew = []}

io :: IO a -> StateT b IO a
io = liftIO

mainMenu :: StateT MainState IO (MenuChoice)
mainMenu = do io $ printOptions
              choice <- (io getUserChoice)
              return choice

printCrew :: StateT MainState IO ()
printCrew = do S {crew = xs} <- get
               io $ printHelper xs
               io $ putStrLn $ "Loaded Crew: " ++ show (length xs)
  where printHelper [] = return ()
        printHelper (x : xs) = do putStrLn (name x)
                                  printHelper xs

doChoice :: MenuChoice -> StateT MainState IO ()
doChoice Display = do S {schedules = (s:xs)} <- get
                      io $ simpleMain $ renderTable $ showSchedule s
doChoice LoadCrew = do io $ putStrLn "Preparing to load crew file"
                       s <- get
                       io $ putStrLn "Enter crew file path(some/in.json): "
                       filePath <- io getLine
                       crew' <- io $ readCrewFromFile filePath
                       put s {crew = crew'}                   
doChoice AddCrew = do additionalCrew <- io $ getMembers []
                      s@(S {crew = crew'}) <- get
                      put $ s {crew = additionalCrew ++ crew'}
doChoice SaveCrew = do io $ putStrLn "Preparing to save to crew file"
                       S {crew = crew'} <- get
                       io $ putStrLn "Enter crew file path(some/out.json): "
                       filePath <- io getLine
                       io $ writeCrewToFile crew' filePath
doChoice ClearCrew = do io $ putStrLn "Clearing crew"
                        s <- get
                        put $ s {crew = []}
doChoice ListCrew = do io $ putStrLn "Listing loaded crew: "
                       printCrew
doChoice GenerateSchedules = do io $ putStrLn "Generating Schedules"
                                s@(S {crew = crew'}) <- get
                                let schedules = generateSchedules crew'
                                let (valid, _) = rankSchedules maximizeCrewUtilizationAndFulldays schedules
                                let formattedValid = map fst valid
                                put $ s {schedules = formattedValid}
doChoice NextSchedule = do io $ putStrLn "Grabbing next schedule"
                           s@(S {schedules = (x : xs)}) <- get
                           put $ s {schedules = (xs ++ [x])}
doChoice Quit = do s <- get
                   put $ s {quit = True}


mainLoop :: StateT MainState IO ()
mainLoop = do choice <- mainMenu
              doChoice choice
              (S {quit = x}) <- get
              if x then return () else mainLoop
-- mainLoop s@(S {quit = x})
--   | x == True = return s
-- mainLoop state = do (state', choice) <- mainMenu state
--                     state'' <- doChoice state choice
--                     mainLoop state''


main :: IO ()
main = do let state = initState
          execStateT mainLoop state
          return ()
          
-- main = do crew <- getMembers []
--           putStrLn "Generating schedule..."
--           let schedule = greedyStrategy crew
--           simpleMain $ renderTable $ showSchedule schedule
