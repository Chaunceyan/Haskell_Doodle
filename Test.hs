import Doodle (Doodle(initialize, add, remove, toogle), Pool(freshKey, get, set), run) 
import Data.List

-- Data time to be Ord
data Time = Time String deriving (Read)

instance Show Time where
	show (Time time) = show time

instance Ord Time where
	(<=) (Time start) (Time end) = (readTime start) <= (readTime end)
	(>=) (Time start) (Time end) = (readTime start) >= (readTime end)
	(<) (Time start) (Time end) = (readTime start) < (readTime end)
	(>) (Time start) (Time end) = (readTime start) > (readTime end)
string2Time :: String -> UTCTime
string2Time t = readTime defaultTimeLocale "%-d-%-m-%Y %l:%M %p" 
-- Data for time slots in Doodle 
data Slot t = Slot { slot :: (t, t)
                 , participants :: [String] }

-- Data and Instance for MyDoodle
data MyDoodle t = MyDoodle { title :: String
                   , slots :: [Slot t] }

instance Ord t => Ord (Slot t) where
	(<=) slot1 slot2 = fst $ slot slot1 <= fst $ slot slot2
	(>=) slot1 slot2 = fst $ slot slot1 >= fst $ slot slot2
	(<) slot1 slot2 = fst $ slot slot1 < fst $ slot slot2
	(>) slot1 slot2 = fst $ slot slot1 > fst $ slot slot2


instance Doodle MyDoodle where
    initialize newTitle = MyDoodle {title = newTitle, slots = []}
    add (s,e) doodle = MyDoodle {title = title doodle, slots = insertSlot (slots doodle) (s,e)} 
    remove key doodle = MyDoodle {title = title doodle, slots = removeNth key slots }
    toogle name key doodle = MyDoodle {title = title doodle, slots = myToogle name key (slots doodle) } 

insertSlot :: (Ord t) =>  [Slot t] -> (t, t) -> [Slot t]
insertSlot slots (start, end) = if noConflict slots (start,end)
								then insert (Slot {slot = (start, end), participants = []}) slots
								else slots
removeNth :: Int -> [Slot] -> [Slot]
removeNth key [] = []
removeNth key slots = let (xs, ys) = splitAt key slots in xs ++ (tail ys)

myToogle :: String -> Int -> [Slot] -> [Slot]
myToogle name key slots = let theSlot = slots !! key in replaceNth key (Slot {slot = slot theSlot, participants = participants theSlot ++ name}) slots

-- Helper function to replace the Nth element.
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

-- Helper function to detect time clash
noConflict slots (start,  end) = all conflictTest slots

conflictTest aSlot (aStart, aEnd)= let (start, end) = slot aSlot in (aStart < start && aEnd <= end || aStart >= end) 


-- Data and Instance for Pool.
data MyPool = MyPool { indentifiers :: [Int]
                     , pairs :: [(Int, MyDoodle String)] }

instance Pool MyPool where
	
 
