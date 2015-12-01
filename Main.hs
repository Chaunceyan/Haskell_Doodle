import Doodle (Doodle(initialize, add, remove, toogle), Pool(freshKey, get, set), run) 

-- Data for time slots in Doodle 
data Slot = Slot { startTime :: String
                 , endTime :: String
                 , participants [String]} deriving Show

-- Data for titles
data Title id  = Title { title :: String
                   , slots [Slot]
                   , id :: id}
instance Doodle emptyDoodle where
    initialize = 

main = run emptyDoodle

