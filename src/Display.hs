{-# LANGUAGE OverloadedStrings #-}
module Display where
import Scheduling (Schedule(..), Assignment (..), Day, CrewMember (name))
import Brick.Widgets.Table (Table, table)
import Data.Map
import Brick
import Data.Maybe
import Data.String


renderRow :: (Day, Assignment) -> [Widget ()]
renderRow (day, A {crew_chief = c, driver = d, thing1 = a1, thing2 = a2}) = [ txt . fromString $ show day
                                                                            , txt . fromString $ maybe "" name c
                                                                            , txt . fromString $ maybe "" name d
                                                                            , txt . fromString $ maybe "" name a1
                                                                            , txt . fromString $ maybe "" name a2]

renderAssignments :: Map Day Assignment -> [[Widget ()]]
renderAssignments a = Prelude.map renderRow (toList a)

showSchedule :: Schedule -> Table ()
showSchedule (Schedule {daily_assignments = assign, crew = _}) =
  table ([txt "Day", txt "Crew Chief", txt "Driver", txt "Attendant", txt "Attendant"] : renderAssignments assign)
        
