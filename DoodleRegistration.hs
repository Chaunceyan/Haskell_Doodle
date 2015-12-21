import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
 
main :: IO ()
main = withSocketsDo $ do 
    channel <- newChan
    -- AF_INET means support Internetwork such as UDP TCP
    -- Stream means it is a Stream sockets which use TCP and SCTP
    mySocket <- socket AF_INET Stream defaultProtocol
    bind mySocket (SockAddrInet 5002 0)
    listen mySocket 5
    loop mySocket channel 0
    
loop :: Socket -> Chan String -> Int -> IO()    
loop socket channel number = do
    connection <- Network.Socket.accept socket
    forkIO $ handleConnection connection channel number
    loop socket channel $ number + 1
    
handleConnection :: (Socket, SockAddr) -> Chan String -> Int -> IO ()
handleConnection (socket, _) channel number = do
    handle <- socketToHandle socket ReadWriteMode
    hSetBuffering handle LineBuffering
    loginInfo <- hGetLine handle
    hPutStrLn handle loginInfo
    hClose handle