import Doodle (Doodle(initialize, add, remove, toogle), Pool(freshKey, get, set), run) 
import Data.List
import System.Locale
import Data.Time 
import Data.Time.Format
import qualified Data.Map.Strict as Map

-- Data time to be Ord
data Time = Time String deriving (Read)

instance Show Time where
	show (Time time) = show time

instance Eq Time where
	(==) (Time start) (Time end) = (string2Time start) == (string2Time end)

instance Ord Time where
	(<=) (Time start) (Time end) = (string2Time start) <= (string2Time end)
	(>=) (Time start) (Time end) = (string2Time start) >= (string2Time end)
	(<) (Time start) (Time end) = (string2Time start) < (string2Time end)
	(>) (Time start) (Time end) = (string2Time start) > (string2Time end)

string2Time :: String -> UTCTime
string2Time t = readTime defaultTimeLocale "%-d-%-m-%Y %l:%M %p" t :: UTCTime

-- Data for time slots in Doodle 
data Slot t = Slot { slot :: (t, t)
                 , participants :: [String] } 

instance Eq t => Eq (Slot t) where
	(==) slot1 slot2 = slot1 == slot2

instance Ord t => Ord (Slot t) where
	(<=) slot1 slot2 = fst (slot slot1) <= fst (slot slot2)
	(>=) slot1 slot2 = fst (slot slot1) >= fst (slot slot2)
	(<) slot1 slot2 = fst (slot slot1) < fst (slot slot2)
	(>) slot1 slot2 = fst (slot slot1) > fst (slot slot2)

-- Data and Instance for MyDoodle
data MyDoodle t = MyDoodle { title :: String
                   , slots :: [Slot t] } 

instance Doodle MyDoodle where
    initialize newTitle = MyDoodle {title = newTitle, slots = []}
    add (s,e) doodle = MyDoodle {title = title doodle, slots = insertSlot (slots doodle) (s,e)} 
    remove key doodle = MyDoodle {title = title doodle, slots = removeNth key (slots doodle) }
    toogle name key doodle = MyDoodle {title = title doodle, slots = myToogle name key (slots doodle) } 

instance Show (MyDoodle t) where
	show doodle = myShow doodle   

insertSlot :: (Ord t) =>  [Slot t] -> (t, t) -> [Slot t]
insertSlot slots (start, end) = if noConflict slots (start,end)
								then insert (Slot {slot = (start, end), participants = []}) slots
								else slots

removeNth :: Int -> [Slot t] -> [Slot t]
removeNth key [] = []
removeNth key slots = let (xs, ys) = splitAt key slots in xs ++ (tail ys)

myToogle :: String -> Int -> [Slot t] -> [Slot t]
myToogle name key slots = let theSlot = slots !! key in replaceNth key ( Slot { slot = slot theSlot, participants = participants theSlot ++ [name] }) slots

myShow :: (MyDoodle Time) -> String
myShow doodle = let
					myTitle = title doodle 
					rows = [(\(start, end) -> [start, end]) (slot s) ++ participants s | s <- (slots doodle) ]
					widths = [ 1 + (length s) + sum [length r | r <- s] | s <- rows]
					separator = "+" ++ intercalate "-+-" [replicate width '-' | width <- widths] ++ "+"
				in 
					unlines $ [separator] ++ concat [ [fillCols row] ++ [separator] | row <- rows ]

-- Helper function to replace the Nth element.
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

-- Helper function to detect time clash
noConflict :: Ord t => [Slot t] -> (t, t) -> Bool
noConflict slots (start,  end) = all (conflictTest (start, end)) slots

conflictTest :: Ord t => (t, t) -> Slot t -> Bool
conflictTest (aStart, aEnd) aSlot = let (start, end) = slot aSlot in (aStart < start && aEnd <= end || aStart >= end) 

-- Helper function to put '|' between strings
fillCols :: [String] -> String
fillCols xs = "| " ++ intercalate " | " xs ++ " |"

-- Data and Instance for Pool.
data MyPool k d = MyPool { keys :: [k]
                     , pairs :: Map.Map k d} deriving Show

instance Pool MyPool where
	freshKey pool =  head $ keys pool
	get key pool = Map.lookup key (pairs pool) 
	set key doodle pool = MyPool { keys = tail (keys pool), pairs = Map.insert key doodle (pairs pool)  }

main :: IO ()
main = run emptyDoodle

emptyDoodle :: MyPool Int (MyDoodle Time)
emptyDoodle = MyPool { keys = [1..], pairs = Map.empty }