import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import System.Environment
import Data.Maybe
import Data.String.Utils
import Data.List
import Data.List.Split

data Identity = Teacher | Student | Administrator deriving (Show, Eq)

data User = User { login :: String, token :: String, identity :: Identity, subscribe :: [(String, (String, String))] } deriving (Show, Eq)
 
data Slot = Slot { slot :: (String, String), scores :: Int } deriving (Show, Eq)

data Doodle = Doodle { name :: String, slots :: [Slot] } deriving (Show, Eq)

main :: IO ()
main = withSocketsDo $ do
    [login, token] <- getArgs
    userList <- newTVarIO ([User {login = login, token = token, identity = Administrator, subscribe = []}])
    doodleList <- newTVarIO ([])
    channel <- newChan
    -- AF_INET means support Internetwork such as UDP TCP
    -- Stream means it is a Stream sockets which use TCP and SCTP
    mySocket <- socket AF_INET Stream defaultProtocol
    bind mySocket (SockAddrInet 5002 0)
    listen mySocket 5
    loop mySocket channel userList doodleList 0
    
--loop :: Socket -> Chan String -> TVar [User] -> Int -> IO ()    
loop socket channel userList doodleList number = do
    (sock, addr) <- Network.Socket.accept socket
    handle <- socketToHandle sock ReadWriteMode
    forkIO $ handleConnection handle channel userList doodleList number
    loop socket channel userList doodleList $ number + 1
    
--handleConnection :: (Socket, SockAddr) -> Chan String -> Int -> IO ()
handleConnection handle channel userList doodleList number = do
    hSetBuffering handle LineBuffering
    hPutStrLn handle loginNotification
    loginInfo <- hGetLine handle
    users <- atomically $ readTVar userList
    let user = checkLogin loginInfo users
    if user /= Nothing 
    then handleRequest (fromJust user) handle userList doodleList
    else do
            hPutStrLn handle "Login Failed."
            handleConnection handle channel userList doodleList number
    hClose handle
    
loginNotification = "Please Login."

-- Code block to handle login
checkLogin loginInfo users = do let [myLogin, myToken] = splitWs loginInfo 
                                find (userLoginTest [myLogin, myToken]) users
    
userLoginTest [myLogin, myToken] user = if (login user) == myLogin && (token user) == myToken
                                        then True 
                                        else False
                                    
handleRequest user handle userList doodleList = do  case identity user of
                                                         Administrator  ->  do  hPutStrLn handle "Login Success. You can do add-teacher, add-student or change-password command."
                                                                                request <- hGetLine handle
                                                                                let inputList = splitWs request
                                                                                case inputList !! 0 of
                                                                                    "add-teacher"       ->  do  users <- atomically $ readTVar userList
                                                                                                                if checkUserExist (inputList !! 1) users 
                                                                                                                then do atomically $ writeTVar userList ([ User {   login = inputList !! 1, 
                                                                                                                                                                    token = inputList !! 2, 
                                                                                                                                                                    identity = Teacher, 
                                                                                                                                                                    subscribe = [] }] ++ users)
                                                                                                                        hPutStrLn handle "Success!"
                                                                                                                else do hPutStrLn handle "User exists!"
                                                                                    "add-student"       ->  do  users <- atomically $ readTVar userList
                                                                                                                if checkUserExist (inputList !! 1) users 
                                                                                                                then do atomically $ writeTVar userList ([ User {   login = inputList !! 1, 
                                                                                                                                                                    token = inputList !! 2, 
                                                                                                                                                                    identity = Student, 
                                                                                                                                                                    subscribe = [] }] ++ users)
                                                                                                                        hPutStrLn handle "Success!"
                                                                                                                else do hPutStrLn handle "User exists!"
                                                                                    "change-password"   ->  do  users <- atomically $ readTVar userList
                                                                                                                let newUsers = delete user users
                                                                                                                atomically $ writeTVar userList (newUsers ++ [ User {   login = login user, 
                                                                                                                                                                        token = inputList !! 1, 
                                                                                                                                                                        identity = identity user, 
                                                                                                                                                                        subscribe = subscribe user }] )
                                                                                    "exam-schedule"     ->  do  doodles <- atomically $ readTVar doodleList
                                                                                                                --printSchedule handle doodles
                                                                                                                hPutStrLn handle $ show doodles
                                                         Teacher        ->  do  hPutStrLn handle "Login Success. You can do set-doodle, get-doodle, exam-schedule or change-password."
                                                                                request <- hGetLine handle
                                                                                let inputList = splitOn " " request
                                                                                case inputList !! 0 of
                                                                                    "set-doodle"        ->  do  doodles <- atomically $ readTVar doodleList
                                                                                                                listOf <-  readDoodle handle
                                                                                                                let slotString = filter ('\t'/=) (filter (' '/=) $ strip listOf)
                                                                                                                let slots = produceSlots $ Data.String.Utils.split "," $ init slotString
                                                                                                                if any (doodleNameTest $ inputList !! 1) doodles
                                                                                                                then hPutStrLn handle "Doodle Exists!"
                                                                                                                else do
                                                                                                                        atomically $ writeTVar doodleList ([Doodle {name = inputList !! 1, slots = slots}])
                                                                                                                        hPutStrLn handle "Doodle set!"
                                                                                    "get-doodle"        ->  do  doodles <- atomically $ readTVar doodleList
                                                                                                                let theDoodle = find (doodleNameTest $ inputList !! 1) doodles
                                                                                                                if theDoodle /= Nothing
                                                                                                                then do printDoodle handle $ slots $ fromJust theDoodle
                                                                                                                else do hPutStrLn handle "Wrong doodle name!"
                                                                                    "change-password"   ->  do  users <- atomically $ readTVar userList
                                                                                                                let newUsers = delete user users
                                                                                                                atomically $ writeTVar userList (newUsers ++ [ User {   login = login user, 
                                                                                                                                                                        token = inputList !! 1, 
                                                                                                                                                                        identity = identity user, 
                                                                                                                                                                        subscribe = subscribe user }])
                                                                                    "exam-schedule"     ->  do  doodles <- atomically $ readTVar doodleList
                                                                                                                --printSchedule handle doodles
                                                                                                                hPutStrLn handle $ show doodles
                                                         Student        ->  do  hPutStrLn handle "Login Success. You can do get-doodle, subscribe, prefer, exam-schedule or change-password."
                                                                                request <- hGetLine handle
                                                                                let inputList = splitOn " " request
                                                                                case inputList !! 0 of 
                                                                                    "get-doodle"        ->  do  doodles <- atomically $ readTVar doodleList
                                                                                                                let theDoodle = find (doodleNameTest $ inputList !! 1) doodles
                                                                                                                if theDoodle /= Nothing
                                                                                                                then do printDoodle handle $ slots $ fromJust theDoodle
                                                                                                                else do hPutStrLn handle "Wrong doodle name!"
                                                                                    "subscribe"         ->  do  doodles <- atomically $ readTVar doodleList
                                                                                                                let theDoodle = find (doodleNameTest $ inputList !! 1) doodles
                                                                                                                if theDoodle /= Nothing
                                                                                                                then do users <- atomically $ readTVar userList
                                                                                                                        let newUsers = delete user users
                                                                                                                        let (start, end) = slot $ (slots $ fromJust theDoodle) !! 0
                                                                                                                        let name = inputList !! 1
                                                                                                                        let [theDoodle] = filter (doodleNameTest name) doodles
                                                                                                                        newSlots <- addScores theDoodle [start, end]
                                                                                                                        let restDoodles = delete theDoodle doodles
                                                                                                                        atomically $ writeTVar doodleList $ restDoodles ++ [Doodle {name = name, slots = newSlots}]
                                                                                                                        atomically $ writeTVar userList (newUsers ++ [ User {   login = login user, 
                                                                                                                                                                                token = token user, 
                                                                                                                                                                                identity = identity user, 
                                                                                                                                                                                subscribe = (subscribe user) ++ [(inputList !! 1, slot ((slots $ theDoodle) !! 0) )]}])
                                                                                                                else hPutStrLn handle "Wrong Doodle Name!"
                                                                                    "prefer"            ->  do  doodles <- atomically $ readTVar doodleList
                                                                                                                let theDoodle = find (doodleNameTest $ inputList !! 1) doodles
                                                                                                                if theDoodle /= Nothing
                                                                                                                then if any (slotExistenceTest (Data.String.Utils.split "/" $ inputList !! 2)) $ slots $ fromJust theDoodle
                                                                                                                     then do 
                                                                                                                             subscriptionList <- updatePreference handle user doodleList (inputList !! 1) $ Data.String.Utils.split "/" (inputList !! 2)
                                                                                                                             users <- atomically $ readTVar userList
                                                                                                                             let newUsers = delete user users
                                                                                                                             hPutStrLn handle $ show user
                                                                                                                             atomically $ writeTVar userList (newUsers ++ [ User {   login = login user, 
                                                                                                                                                                                     token = token user, 
                                                                                                                                                                                     identity = identity user, 
                                                                                                                                                                                     subscribe = subscriptionList }] )
                                                                                                                     else hPutStrLn handle "Slot doesn't exist!"
                                                                                                                else hPutStrLn handle "Wrong doodle name!"
                                                                                    "change-password"   ->  do  users <- atomically $ readTVar userList
                                                                                                                let newUsers = delete user users
                                                                                                                atomically $ writeTVar userList (newUsers ++ [ User {   login = login user, 
                                                                                                                                                                        token = inputList !! 1, 
                                                                                                                                                                        identity = identity user, 
                                                                                                                                                                        subscribe = subscribe user }] )
                                                                                    "exam-schedule"     ->  do  doodles <- atomically $ readTVar doodleList
                                                                                                                printSchedules handle doodles
                                                                                                                hPutStrLn handle $ show doodles
                                                    users <- atomically $ readTVar userList
                                                    let updateUser = fromJust $ find (userLoginTest [login user, token user]) users
                                                    -- testing block
                                                    doodles <- atomically $ readTVar doodleList
                                                    hPutStrLn handle $ show doodles
                                                    -- testing block ends
                                                    handleRequest updateUser handle userList doodleList
                                                
checkUserExist login users = all (userExitTest login) users

userExitTest myLogin user = myLogin /= login user 

readDoodle handle = do
                    rest <- hGetLine handle
                    if (endswith "]" $ rstrip rest)
                    then return rest
                    else liftM (rest ++) (readDoodle handle)

produceSlots [] = []

produceSlots (slot:slots) = [Slot {slot = (start, end), scores = 0}] ++ (produceSlots slots)
                        where 
                            [start, end] = splitOn "/" slot
                            
doodleNameTest doodleName doodle = doodleName == name doodle

printDoodle handle (doodle:[]) = do let (start, end) = slot doodle
                                    hPutStrLn handle $ "\t" ++ start ++ " / " ++ end
                                    hPutStrLn handle "]"

printDoodle handle (doodle:doodles) = do    let (start, end) = slot doodle
                                            hPutStrLn handle "ok ["
                                            hPutStrLn handle $ "\t" ++ start ++ " / " ++ end ++ ","
                                            printDoodle handle doodles
                                            
slotExistenceTest [start, end] mySlot = (start, end) == slot mySlot

updatePreference handle user doodleList name [start, end] = do  let subscribeList = subscribe user
                                                                if all (subscriptionListTest name) subscribeList
                                                                then do hPutStrLn handle "Not subscribed"
                                                                        return $ subscribe user
                                                                else do 
                                                                        let (oldName, (oldStart, oldEnd)) = fromJust $ find (subscriptionListNegateTest name) subscribeList 
                                                                        doodles <- atomically $ readTVar doodleList
                                                                        let [theDoodle] = filter (doodleNameTest name) doodles
                                                                        addedSlots <- addScores theDoodle [start, end]
                                                                        newSlots <- minusScores (Doodle {name = name, slots = addedSlots}) [oldStart, oldEnd]
                                                                        let restDoodles = delete theDoodle doodles
                                                                        atomically $ writeTVar doodleList $ restDoodles ++ [Doodle {name = name, slots = newSlots}]
                                                                        return $ (filter (subscriptionListTest name) subscribeList) ++ [(name, (start, end))]
                                                    
subscriptionListTest name (doodleName, slot) = name /= doodleName
subscriptionListNegateTest name (doodleName, slot) = name == doodleName

addScores theDoodle [start, end] = do 
                                    let [theSlot] = filter (slotExistenceTest [start, end]) $ slots theDoodle
                                    let theRestSlots = delete theSlot $ slots theDoodle
                                    let newScore = 1 + scores theSlot
                                    return $ theRestSlots ++ [ Slot { slot = (start, end), scores = newScore }]
                                    
minusScores theDoodle [start, end] = do 
                                    let [theSlot] = filter (slotExistenceTest [start, end]) $ slots theDoodle
                                    let theRestSlots = delete theSlot $ slots theDoodle
                                    let newScore = scores theSlot - 1
                                    return $ theRestSlots ++ [ Slot { slot = (start, end), scores = newScore }]
                                    
printSchedules handle (doodle:doodles)
    | length doodles /= 0 = do  printSchedule handle doodle
                                printSchedules handle doodles
    | otherwise           = do  printSchedule handle doodle
    
printSchedule handle doodle = do    let (start, end) = slot $ maximumBy (compareSlotPreference) (slots doodle) 
                                    hPutStrLn handle $ "{"++name doodle++" : "++start++"/"++end++"}" 

compareSlotPreference lSlot rSlot = compare (scores lSlot) (scores rSlot)