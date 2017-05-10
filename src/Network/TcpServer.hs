module Network.TcpServer
    (
        TcpServer, newServer, shutdownServer, waitListen
    ) where

import           Control.Concurrent.MVar      (MVar, newEmptyMVar, putMVar,
                                               readMVar)
import           Control.Exception            (bracket, finally)
import           Control.Monad                (void)
import           Network.Socket               (Family (..), PortNumber,
                                               SockAddr (..), Socket,
                                               SocketOption (..),
                                               SocketType (..), accept, bind,
                                               close, defaultProtocol,
                                               iNADDR_ANY, listen,
                                               setSocketOption, socket)

import           Control.Concurrent.Hierarchy

data TcpServer = TcpServer ThreadMap (MVar ())

newServer :: PortNumber -> (ThreadMap -> Socket -> IO ()) -> IO TcpServer
newServer port handler = do
    readyToConnect <- newEmptyMVar
    rootChildren <- newRoot $ \brothers ->
        bracket (newListener port readyToConnect) close (acceptLoop brothers handler)
    return $ TcpServer rootChildren readyToConnect

waitListen :: TcpServer -> IO ()
waitListen (TcpServer _ readyToConnect) = readMVar readyToConnect >>= \_ -> return ()

shutdownServer :: TcpServer -> IO ()
shutdownServer (TcpServer rootChildren _) = shutdownRoot rootChildren


newListener :: PortNumber -> MVar() -> IO Socket
newListener port readyToConnect = do
    sk <- socket AF_INET Stream defaultProtocol
    setSocketOption sk ReuseAddr 1
    bind sk (SockAddrInet port iNADDR_ANY)
    listen sk 5
    putMVar readyToConnect ()
    return sk

acceptLoop :: ThreadMap -> (ThreadMap -> Socket -> IO ()) -> Socket -> IO ()
acceptLoop brothers handler sk = go
  where
    go = do
        (peer, _) <- accept sk
        void $ newChild brothers $ \brothersOfChild -> finally (handler brothersOfChild peer) (close peer)
        go
