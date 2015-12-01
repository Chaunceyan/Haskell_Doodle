import Doodle (Doodle(initialize, add, remove, toogle), Pool(freshKey, get, set), run) 

-- Data for time slots in Doodle 
data Slot = Slot { startTime :: String
                 , endTime :: String
                 , participants [String] } deriving Show

-- Data and Instance for MyDoodle
data MyDoodle id  = MyDoodle { title :: String
                   , slots [Slot] }

instance Doodle MyDoodle where
    initialize newTitle = MyDoodle { title = newTitle
                                   , slots = [] 
                                   , participants = [] }
    add slot = slots : slot
    remove participate = participants index participants 

-- Data and Instance for Pool
data MyPool = MyPool { indentifiers = [1..]
                     , pairs = [(Int, MyDoodle)] }

instance Pool MyPool where
    freshKey MyPool = key (MyDoodle) 
    get key = 


