{-# LANGUAGE ScopedTypeVariables #-}

module Network.TcpServer
    (
      TcpServer
    , newServer
    , shutdownServer
    , waitListen
    , ThreadMap
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

import           Control.Concurrent.Hierarchy (ThreadMap, newChild,
                                               newThreadMap, shutdown)

data TcpServer = TcpServer ThreadMap (MVar ())

newServer :: PortNumber -> (ThreadMap -> Socket -> IO ()) -> IO TcpServer
newServer port handler = do
    readyToConnect <- newEmptyMVar
    rootThreadMap <- newThreadMap
    void $ newChild rootThreadMap $ \listenerChildren ->
        bracket (newListener port readyToConnect) close (acceptLoop listenerChildren handler)
    return $ TcpServer rootThreadMap readyToConnect

waitListen :: TcpServer -> IO ()
waitListen (TcpServer _ readyToConnect) = readMVar readyToConnect >>= \_ -> return ()

shutdownServer :: TcpServer -> IO ()
shutdownServer (TcpServer rootThreadMap _) = shutdown rootThreadMap


newListener :: PortNumber -> MVar() -> IO Socket
newListener port readyToConnect = do
    sk <- socket AF_INET Stream defaultProtocol
    setSocketOption sk ReuseAddr 1
    bind sk (SockAddrInet port iNADDR_ANY)
    listen sk 5
    putMVar readyToConnect ()
    return sk

acceptLoop :: ThreadMap -> (ThreadMap -> Socket -> IO ()) -> Socket -> IO ()
acceptLoop listenerChildren handler sk = go
  where
    go = do
        (peer, _) <- accept sk
        void $ newChild listenerChildren $ \handlerChildren -> finally (handler handlerChildren peer) (close peer)
        go
