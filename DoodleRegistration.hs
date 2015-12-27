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
import Data.List
import Data.List.Split

data Identity = Teacher | Student | Administrator deriving (Show, Eq)

data User = User { login :: String, token :: String, identity :: Identity } deriving (Show, Eq)
 
main :: IO ()
main = withSocketsDo $ do
    [login, token] <- getArgs
    userLists <- newTVarIO ([User {login = login, token = token, identity = Administrator}])
    channel <- newChan
    -- AF_INET means support Internetwork such as UDP TCP
    -- Stream means it is a Stream sockets which use TCP and SCTP
    mySocket <- socket AF_INET Stream defaultProtocol
    bind mySocket (SockAddrInet 5002 0)
    listen mySocket 5
    loop mySocket channel userLists 0
    
loop :: Socket -> Chan String -> TVar [User] -> Int -> IO ()    
loop socket channel userLists number = do
    (sock, addr) <- Network.Socket.accept socket
    handle <- socketToHandle sock ReadWriteMode
    forkIO $ handleConnection handle channel userLists number
    loop socket channel userLists $ number + 1
    
--handleConnection :: (Socket, SockAddr) -> Chan String -> Int -> IO ()
handleConnection handle channel userLists number = do
    hSetBuffering handle LineBuffering
    hPutStrLn handle loginNotification
    loginInfo <- hGetLine handle
    users <- atomically $ readTVar userLists
    let user = checkLogin loginInfo users
    if user /= Nothing 
    then handleRequest (identity $ fromJust user) handle userLists
    else do
            hPutStrLn handle "Login Failed."
            handleConnection handle channel userLists number
    hClose handle
    
loginNotification = "Please Login."

-- Code block to handle login
checkLogin loginInfo users = do let [myLogin, myToken] = splitOn " " loginInfo 
                                find (userLoginTest [myLogin, myToken]) users
    
userLoginTest [myLogin, myToken] user = if (login user) == myLogin && (token user) == myToken
                                        then True 
                                        else False
                                    
handleRequest myIdentity handle userLists = do  case myIdentity of
                                                    Administrator -> do hPutStrLn handle "Login Success. You can do add-teacher or add-student command."
                                                                        request <- hGetLine handle
                                                                        let inputList = splitOn " " request
                                                                        case inputList !! 0 of
                                                                            "add-teacher" -> do users <- atomically $ readTVar userLists
                                                                                                if checkUserExist (inputList !! 1) users 
                                                                                                then do atomically $ writeTVar userLists ([ User {login = inputList !! 1, token = inputList !! 2, identity = Teacher}] ++ users)
                                                                                                        hPutStrLn handle "Success!"
                                                                                                else do hPutStrLn handle "User exists!"
                                                                            "add-student" -> do users <- atomically $ readTVar userLists
                                                                                                if checkUserExist (inputList !! 1) users 
                                                                                                then do atomically $ writeTVar userLists ([ User {login = inputList !! 1, token = inputList !! 2, identity = Student}] ++ users)
                                                                                                        hPutStrLn handle "Success!"
                                                                                                else do hPutStrLn handle "User exists!"

                                                    Teacher -> do hPutStrLn handle "Login Success. You can do set-doodle, get-doodle, exam-schedule and change-password."
                                                                  request <- hGetLine handle
                                                                  let inputList = splitOn " " request
                                                                  case inputList !! 0 of
                                                                      "set-doodle" -> do hPutStrLn handle "To be implement!"
                                                                      "get-doodle" -> do hPutStrLn handle "To be implement!"
                                                                      "exam-schedule" -> do hPutStrLn handle "To be implement!"
                                                                      "change-password" -> do hPutStrLn handle "To be implement!"
                                                    Student -> do hPutStrLn handle "To be implement!"
                                                handleRequest myIdentity handle userLists
                                                
checkUserExist login users = all (userExitTest login) users

userExitTest myLogin user = myLogin /= login user 