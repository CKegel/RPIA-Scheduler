module Persistence where
import Scheduling (CrewMember)
import Data.Aeson
import Data.ByteString.Lazy
import Data.Maybe


-- | readCrewFromFile reads the list of crew memebers from a file.
readCrewFromFile :: FilePath -> IO [CrewMember]
readCrewFromFile path = unwrap . decode <$> Data.ByteString.Lazy.readFile path
  where unwrap (Just x) = x
        unwrap Nothing = []

writeCrewToFile :: [CrewMember] -> FilePath -> IO ()
writeCrewToFile crew path = Data.ByteString.Lazy.writeFile path encodedStr
  where encodedStr = encode crew
