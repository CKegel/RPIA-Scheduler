module Main (main) where

import Lib
import System.IO
import Scheduling (CrewMember(..), Credential(..), Day(..), newMember, greedyStrategy, Schedule)
import Display
import Brick.Widgets.Table
import Brick


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

main :: IO ()
main = do crew <- getMembers []
          putStrLn "Generating schedule..."
          let schedule = greedyStrategy crew
          simpleMain $ renderTable $ showSchedule schedule
