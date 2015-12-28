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

data User = User { login :: String, token :: String, identity :: Identity } deriving (Show, Eq)
 
data Slot = Slot { slot :: (String, String) } deriving (Show)

data Doodle = Doodle { name :: String, slots :: [Slot] } deriving (Show)

main :: IO ()
main = withSocketsDo $ do
    [login, token] <- getArgs
    userList <- newTVarIO ([User {login = login, token = token, identity = Administrator}])
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
                                                         Administrator  -> do   hPutStrLn handle "Login Success. You can do add-teacher, add-student or change-password command."
                                                                                request <- hGetLine handle
                                                                                let inputList = splitWs request
                                                                                case inputList !! 0 of
                                                                                    "add-teacher"       -> do   users <- atomically $ readTVar userList
                                                                                                                if checkUserExist (inputList !! 1) users 
                                                                                                                then do atomically $ writeTVar userList ([ User {login = inputList !! 1, token = inputList !! 2, identity = Teacher}] ++ users)
                                                                                                                        hPutStrLn handle "Success!"
                                                                                                                else do hPutStrLn handle "User exists!"
                                                                                    "add-student"       -> do   users <- atomically $ readTVar userList
                                                                                                                if checkUserExist (inputList !! 1) users 
                                                                                                                then do atomically $ writeTVar userList ([ User {login = inputList !! 1, token = inputList !! 2, identity = Student}] ++ users)
                                                                                                                        hPutStrLn handle "Success!"
                                                                                                                else do hPutStrLn handle "User exists!"
                                                                                    "change-password"   -> do   users <- atomically $ readTVar userList
                                                                                                                let newUsers = delete user users
                                                                                                                atomically $ writeTVar userList (newUsers ++ [ User {login = login user, token = inputList !! 1, identity = identity user}] )
                                                         Teacher        -> do   hPutStrLn handle "Login Success. You can do set-doodle, get-doodle, exam-schedule or change-password."
                                                                                request <- hGetLine handle
                                                                                let inputList = splitOn " " request
                                                                                case inputList !! 0 of
                                                                                    "set-doodle"        -> do   doodles <- atomically $ readTVar doodleList
                                                                                                                listOf <-  readDoodle handle
                                                                                                                let slotString = filter ('\t'/=) (filter (' '/=) $ strip listOf)
                                                                                                                let slots = produceSlots $ Data.String.Utils.split "," $ init slotString
                                                                                                                atomically $ writeTVar doodleList ([Doodle {name = inputList !! 1, slots = slots}])
                                                                                                                hPutStrLn handle "Doodle set!"
                                                                                    "get-doodle"        -> do   doodles <- atomically $ readTVar doodleList
                                                                                                                hPutStrLn handle $ show doodles
                                                                                    "exam-schedule"     -> do   hPutStrLn handle "To be implement!"
                                                                                    "change-password"   -> do   users <- atomically $ readTVar userList
                                                                                                                let newUsers = delete user users
                                                                                                                atomically $ writeTVar userList (newUsers ++ [ User {login = login user, token = inputList !! 1, identity = identity user}] )
                                                         Student        -> do   hPutStrLn handle "Login Success. You can do get-doodle, subscribe, prefer, exam-schedule or change-password."
                                                                                request <- hGetLine handle
                                                                                let inputList = splitOn " " request
                                                                                case inputList !! 0 of 
                                                                                    "change-password" -> do users <- atomically $ readTVar userList
                                                                                                            let newUsers = delete user users
                                                                                                            atomically $ writeTVar userList (newUsers ++ [ User {login = login user, token = inputList !! 1, identity = identity user}] )
                                                    handleRequest user handle userList doodleList
                                                
checkUserExist login users = all (userExitTest login) users

userExitTest myLogin user = myLogin /= login user 

readDoodle handle = do
                    rest <- hGetLine handle
                    if (endswith "]" $ rstrip rest)
                    then return rest
                    else liftM (rest ++) (readDoodle handle)

produceSlots [] = []

produceSlots (slot:slots) = [Slot {slot = (start, end)}] ++ (produceSlots slots)
                        where 
                            [start, end] = splitOn "/" slot