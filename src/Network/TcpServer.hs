{-# LANGUAGE ScopedTypeVariables #-}


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
import           Control.Monad        (forever, unless)
import qualified Data.ByteString      as B (null)
import qualified Data.ByteString.Lazy as BL (fromStrict)

import           Network.TcpServer

newEchoServer :: IO TcpServer
newEchoServer = newTlsServer 8443 echoServerHandler

echoServerHandler :: TransportHandler
echoServerHandler _ peer = go
  where
    go = do
        msg <- transportRecv peer
        unless (B.null msg) $ do
            transportSend peer $ BL.fromStrict msg
            go
@

-}
module Network.TcpServer
    (
    -- * Types
      TransportHandler
    , TcpServer
    , Transport
    -- * Functions
    , newTcpServer
    , newTlsServer
    , shutdownServer
    , waitListen
    , transportRecv
    , transportSend
    -- * Re-exported Types
    , ThreadMap
    ) where


import           Control.Concurrent.MVar        (MVar, newEmptyMVar, putMVar,
                                                 readMVar)
import           Control.Exception              (IOException, SomeException,
                                                 bracket, catch, finally,
                                                 throwIO)
import           Control.Monad                  (forever, void)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy           as L (ByteString, fromStrict)
import           Network.Socket                 (Family (..), PortNumber,
                                                 ShutdownCmd (..),
                                                 SockAddr (..), Socket,
                                                 SocketOption (..),
                                                 SocketType (..), accept, bind,
                                                 close, defaultProtocol,
                                                 iNADDR_ANY, listen,
                                                 setSocketOption, shutdown,
                                                 socket)
import qualified Network.Socket.ByteString      (recv)
import qualified Network.Socket.ByteString.Lazy (sendAll)
import           Network.TLS                    (Context, ServerParams (..),
                                                 bye, contextNew, handshake,
                                                 recvData, sendData)

import           Data.Default.Class

import           Control.Concurrent.Hierarchy   (ThreadMap, killThreadHierarchy,
                                                 newChild, newThreadMap)

{-|
    'Transport' is abstracted connection to remote peer.  The real backend can be bare TCP socket
    or TLS context.  'Transport' is passed to user provided connection handler.  User can read and write its
    contents via 'transportRecv' and 'transportSend' functions regardless which backend is actually used.
-}
data Transport = TransportSocket Socket | TransportTls Context

{-|
    Receive a chunk of data from the 'Transport'.  If the remote terminated the connection, zero length
    'ByteString' is returned.
-}
transportRecv :: Transport -> IO ByteString
transportRecv (TransportSocket sk) = Network.Socket.ByteString.recv sk 4096
transportRecv (TransportTls ctx)   = recvData ctx

{-|
    Send all content of the 'ByteString' to the 'Transport'.  The 'ByteString' must be lazy ByteString.
-}
transportSend :: Transport -> L.ByteString -> IO ()
transportSend (TransportSocket sk) msg  = Network.Socket.ByteString.Lazy.sendAll sk msg
transportSend (TransportTls ctx) msg    = sendData ctx msg

{-# DEPRECATED TcpHandler "use TransportHandler" #-}
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
    'TransportHandler' is type synonym of user provided function which actually working with single connection.
    The connection can be nicked TCP connection or already established TLS connection.
    Your handler is called with 'ThreadMap' coming from thread-hierarchy package.
    You can ignore it as long as your handler doesn't create its own child thread.
    If the handler creates child, you must use 'newChild' in thread-hierarchy with passing the given 'ThreadMap'
    as the first argument of 'newChild' so that 'newChild' can properly install cleanup routine on exit of
    your handler or entire server.
-}
type TransportHandler
    =  ThreadMap    -- ^ Empty ThreadMap to be used when the handler creates its child thread with newChild.
    -> Transport    -- ^ Abstracted connection to peer which the handler working with.
    -> IO ()

makeTcpHandler :: TransportHandler -> (ThreadMap -> Socket -> IO ())
makeTcpHandler handler handlerChildren peer = handler handlerChildren $ TransportSocket peer

makeTlsHandler :: ServerParams -> TransportHandler -> (ThreadMap -> Socket -> IO ())
makeTlsHandler params handler handlerChildren peer =
    bracket
        (do
            ctx <- contextNew peer params
            handshake ctx
            return ctx)
        (\ctx ->
            -- If an async exception was thrown, say killThread, we still have active TLS context.
            -- So we need to try to gracefully shutdown it.
            -- However, there is some possibility that the context was already invalidated when we reached here.
            -- For example, if user provided handler returned because it detected EoF on receiving data,
            -- the context can be already invalid and close notification may result error.
            -- Causing IOException by trying to close already invalid context is not a problem for us,
            -- so we just catch the exception then ignore it.
            bye ctx `catch` \(_ :: IOException) -> return ()
        )
        (handler handlerChildren . TransportTls)

{-|
    A simple plain TCP server.
-}
data TcpServer = TcpServer ThreadMap (MVar ())

{-|
    Create a new 'TcpServer' with non-secure transport.  It forks a new thread to listen given TCP port.
    It calls user provided 'TransportHandler' every time it accepts new TCP connection from peer.
    The 'TransportHandler' is executed by its dedicated thread.
-}
newTcpServer
    :: PortNumber       -- ^ TCP port number the newly created server to listen.
    -> TransportHandler -- ^ User provided function handling abstracted transport.
    -> IO TcpServer     -- ^ newTcpSever returns created server object.
newTcpServer port handler = newServer port $ makeTcpHandler handler

{-|
    Create a new 'TcpServer' with TLS transport.  It forks a new thread to listen given TCP port.
    It calls user provided 'TransportHandler' every time it accepts new TCP connection from peer.
    The 'TransportHandler' is executed by its dedicated thread.
-}
newTlsServer
    :: ServerParams     -- ^ Parameters for TLS server.
    -> PortNumber       -- ^ TCP port number the newly created server to listen.
    -> TransportHandler -- ^ User provided function handling abstracted transport.
    -> IO TcpServer     -- ^ newTlsSever returns created server object.
newTlsServer params port handler = newServer port $ makeTlsHandler params handler


{-# DEPRECATED newServer "use newTcpServer or newTlsServer" #-}
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
        bracket (newListener port readyToConnect) (close) (acceptLoop listenerChildren handler)
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
shutdownServer (TcpServer rootThreadMap _) = killThreadHierarchy rootThreadMap


newListener :: PortNumber -> MVar() -> IO Socket
newListener port readyToConnect = do
    sk <- socket AF_INET Stream defaultProtocol
    setSocketOption sk ReuseAddr 1
    bind sk (SockAddrInet port iNADDR_ANY)
    listen sk 10000
    return sk
    putMVar readyToConnect ()
    return sk

acceptLoop :: ThreadMap -> TcpHandler -> Socket -> IO ()
acceptLoop listenerChildren handler sk = forever $ do
    (peer, _) <- accept sk
    void . newChild listenerChildren $ \handlerChildren ->
        handler handlerChildren peer `finally` (do
            -- When we reached here, there are two possible case.
            -- (1) We received asynchronous exception
            -- In this case, we should try to shutdown the socket gracefully.
            -- (2) User provided handler returned because it has no remaining tasks (e.g. EoF detected)
            -- In this case, the socket can be terminated by remote.  Trying to shutdown the socket may
            -- fail but we can ignore such failure.
            shutdown peer ShutdownSend `catch` (\(_ :: IOException) -> return ())
            close peer)
