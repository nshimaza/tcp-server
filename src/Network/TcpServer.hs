{-|
Module      : Network.TcpServer
Copyright   : (c) Naoto Shimazaki 2017
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

A Simple Plain TCP Server.

Boot a simple, plain, general purpose TCP server with automatic hierarchical thread management.
It accepts a user provided handler and forks dedicated thread which listens given TCP port number.
Every time the listening thread accept a new connection from peer, it forks new thread dedicated for
each connection then calls user provided handler.

When server shutdown, all threads including listening thread, handler threads and their children
automatically killed.  You don't have to close the socket by your own.  This module automatically
close the socket on handler exit regardless if it exited normally or it was killed.
You can install your own cleanup routine using finally or bracket.


=== Example

@
newEchoServer :: IO TcpServer
newEchoServer = newServer 8080 echoServerHandler

echoServerHandler :: ThreadMap -> Socket -> IO ()
echoServerHandler _ peer = go
  where
    go = do
        msg <- recv peer 4096
        unless (null msg) $ do
            sendAll peer msg
            go
@

-}
module Network.TcpServer
    (
    -- * Types
      TcpHandler
    , TcpServer
    -- * Functions
    , newServer
    , shutdownServer
    , waitListen
    -- * Re-exported Types
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

{-|
    'TcpHandler' is type synonym of user provided function which actually working with single TCP connection.
    Your handler is called with 'ThreadMap' coming from thread-hierarchy package.
    You can ignore it as long as your handler doesn't create its own child thread.
    If the handler creates child, you must use 'newChild' in thread-hierarchy with passing the given 'ThreadMap'
    as the first argument of 'newChild' so that 'newChild' can properly install cleanup routine on exit of
    your handler or entire server.
-}
type TcpHandler
    =  ThreadMap    -- ^ Empty ThreadMap to be used when the handler creates its child thread with newChild.
    -> Socket       -- ^ TCP socket connected from peer which the handler working with.
    -> IO ()

{-|
    A simple plain TCP server.
-}
data TcpServer = TcpServer ThreadMap (MVar ())

{-|
    Create a new 'TcpServer'.  It forks a new thread to listen given TCP port.
    It calls user provided 'TcpHandler' every time it accepts new TCP connection from peer.
    The 'TcpHandler' is executed by its dedicated thread.
-}
newServer
    :: PortNumber   -- ^ TCP port number the newly created server to listen.
    -> TcpHandler   -- ^ User provided function handling each TCP connection.
    -> IO TcpServer -- ^ newSever returns created server object.
newServer port handler = do
    readyToConnect <- newEmptyMVar
    rootThreadMap <- newThreadMap
    void . newChild rootThreadMap $ \listenerChildren ->
        bracket (newListener port readyToConnect) close (acceptLoop listenerChildren handler)
    return $ TcpServer rootThreadMap readyToConnect

{-|
    Wait for given 'TcpServer' ready to accept new connection from peer.
    This is useful for unit testing where you can synchronize client connection to server boot up.
-}
waitListen :: TcpServer -> IO ()
waitListen (TcpServer _ readyToConnect) = readMVar readyToConnect >>= \_ -> return ()

{-|
    Shutdown given 'TcpServer'.  It kills all threads of its children, grandchildren and so on.
-}
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
        void . newChild listenerChildren $ \handlerChildren -> finally (handler handlerChildren peer) (close peer)
        go
