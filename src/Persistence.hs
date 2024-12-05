module Persistence where
import Scheduling (CrewMember)

-- | readCrewFromFile reads the list of crew memebers from a file.
readCrewFromFile :: FilePath -> IO [CrewMember]
readCrewFromFile = undefined

crewToJSON :: CrewMember -> JSONValue 
crewToJSON = undefined

crewFromJSON :: JSONValue -> Maybe CrewMember
crewFromJSON = undefined