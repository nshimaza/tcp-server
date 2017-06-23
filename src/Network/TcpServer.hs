{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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


import           Control.Concurrent.MVar.Lifted (MVar, newEmptyMVar, putMVar,
                                                 readMVar)
import           Control.Exception.Lifted       (bracket, finally)
import           Control.Monad                  (forever, void)
import           Control.Monad.Base             (MonadBase, liftBase)
import           Control.Monad.Trans.Control    (MonadBaseControl)
import           Data.Default.Class             (def)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy           as L (ByteString, fromStrict)
import           Network.Socket                 (Family (..), PortNumber,
                                                 SockAddr (..), Socket,
                                                 SocketOption (..),
                                                 SocketType (..), accept, bind,
                                                 close, defaultProtocol,
                                                 iNADDR_ANY, listen,
                                                 setSocketOption, socket)
import qualified Network.Socket.ByteString      (recv)
import qualified Network.Socket.ByteString.Lazy (sendAll)
import           Network.TLS                    (Context, ServerParams (..), bye, contextNew, handshake,
                                                 recvData, sendData)

import           Control.Concurrent.Hierarchy   (ThreadMap, newChild,
                                                 newThreadMap, shutdown)


data Transport = TransportSocket Socket | TransportTls Context

transportRecv :: MonadBase IO m => Transport -> m ByteString
transportRecv (TransportSocket sk)  = liftBase $ Network.Socket.ByteString.recv sk 4096
transportRecv (TransportTls ctx)    = (liftBase . recvData) ctx

transportSend :: MonadBase IO m => Transport -> L.ByteString -> m ()
transportSend (TransportSocket sk)  = liftBase . Network.Socket.ByteString.Lazy.sendAll sk
transportSend (TransportTls ctx)    = liftBase . sendData ctx

{-|
    'TcpHandler' is type synonym of user provided function which actually working with single TCP connection.
    Your handler is called with 'ThreadMap' coming from thread-hierarchy package.
    You can ignore it as long as your handler doesn't create its own child thread.
    If the handler creates child, you must use 'newChild' in thread-hierarchy with passing the given 'ThreadMap'
    as the first argument of 'newChild' so that 'newChild' can properly install cleanup routine on exit of
    your handler or entire server.
-}
type TcpHandler m
    =  ThreadMap    -- ^ Empty ThreadMap to be used when the handler creates its child thread with newChild.
    -> Socket       -- ^ TCP socket connected from peer which the handler working with.
    -> m ()

type TransportHandler m
    =  ThreadMap    -- ^ Empty ThreadMap to be used when the handler creates its child thread with newChild.
    -> Transport    -- ^ TCP socket connected from peer which the handler working with.
    -> m ()

makeTcpHandler :: MonadBaseControl IO m => TransportHandler m -> (ThreadMap -> Socket -> m ())
makeTcpHandler handler handlerChildren peer = handler handlerChildren $ TransportSocket peer

makeTlsHandler :: MonadBaseControl IO m => ServerParams -> TransportHandler m -> (ThreadMap -> Socket -> m ())
makeTlsHandler params handler handlerChildren peer =
    bracket
        (liftBase $ do
            ctx <- contextNew peer params
            handshake ctx
            return ctx)
        (handler handlerChildren . TransportTls)
        (liftBase . bye)

{-|
    A simple plain TCP server.
-}
data TcpServer = TcpServer ThreadMap (MVar ())

newTcpServer
    :: MonadBaseControl IO m
    => PortNumber           -- ^ TCP port number the newly created server to listen.
    -> TransportHandler m   -- ^ User provided function handling abstracted transport.
    -> m TcpServer          -- ^ newTcpSever returns created server object.
newTcpServer port handler = newServer port $ makeTcpHandler handler

newTlsServer
    :: MonadBaseControl IO m
    => ServerParams         -- ^ Parameters for TLS server.
    -> PortNumber           -- ^ TCP port number the newly created server to listen.
    -> TransportHandler m   -- ^ User provided function handling abstracted transport.
    -> m TcpServer          -- ^ newTlsSever returns created server object.
newTlsServer params port handler = newServer port $ makeTlsHandler params handler



{-|
    Create a new 'TcpServer'.  It forks a new thread to listen given TCP port.
    It calls user provided 'TcpHandler' every time it accepts new TCP connection from peer.
    The 'TcpHandler' is executed by its dedicated thread.
-}
newServer
    :: MonadBaseControl IO m
    => PortNumber   -- ^ TCP port number the newly created server to listen.
    -> TcpHandler m -- ^ User provided function handling each TCP connection.
    -> m TcpServer  -- ^ newSever returns created server object.
newServer port handler = do
    readyToConnect <- newEmptyMVar
    rootThreadMap <- newThreadMap
    void . newChild rootThreadMap $ \listenerChildren ->
        bracket (newListener port readyToConnect) (liftBase . close) (acceptLoop listenerChildren handler)
    return $ TcpServer rootThreadMap readyToConnect

{-|
    Wait for given 'TcpServer' ready to accept new connection from peer.
    This is useful for unit testing where you can synchronize client connection to server boot up.
-}
waitListen :: MonadBase IO m => TcpServer -> m ()
waitListen (TcpServer _ readyToConnect) = readMVar readyToConnect >>= \_ -> return ()

{-|
    Shutdown given 'TcpServer'.  It kills all threads of its children, grandchildren and so on.
-}
shutdownServer :: MonadBase IO m => TcpServer -> m ()
shutdownServer (TcpServer rootThreadMap _) = shutdown rootThreadMap


newListener :: MonadBase IO m => PortNumber -> MVar() -> m Socket
newListener port readyToConnect = do
    sk <- liftBase $ do
        sk <- socket AF_INET Stream defaultProtocol
        setSocketOption sk ReuseAddr 1
        bind sk (SockAddrInet port iNADDR_ANY)
        listen sk 5
        return sk
    putMVar readyToConnect ()
    return sk

acceptLoop :: MonadBaseControl IO m => ThreadMap -> TcpHandler m -> Socket -> m ()
acceptLoop listenerChildren handler sk = forever $ do
    (peer, _) <- liftBase $ accept sk
    void . newChild listenerChildren $ \handlerChildren ->
        handler handlerChildren peer `finally`  (liftBase . close) peer
