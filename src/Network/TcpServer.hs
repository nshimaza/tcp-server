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
      Transport
    , TcpServerConfig (..)
    -- * Functions
    , newTcpServer
    , newTlsServer
    , recv
    , send
    ) where

import           Control.Monad                  (forever, replicateM_, void)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy           as L (ByteString, fromStrict)
import           Data.Default.Class
import           Data.Functor                   (($>))
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
import           UnliftIO

import           Control.Concurrent.Supervisor

class Transport t where
    recv :: t -> IO ByteString
    send :: t -> L.ByteString -> IO ()

instance Transport Socket where
    recv sk = Network.Socket.ByteString.recv sk 4096
    send    = Network.Socket.ByteString.Lazy.sendAll

instance Transport Context where
    recv    = recvData
    send    = sendData

data TcpServerConfig = TcpServerConfig
    { tcpServerConfigPort           :: PortNumber
    , tcpServerConfigBacklog        :: Int
    , tcpServerConfigNumWorkers     :: Int
    , tcpServerConfigTlsParams      :: ServerParams
    , tcpServerConfigBeforeMainLoop :: IO ()
    }

instance Default TcpServerConfig where
    def = TcpServerConfig 9000 10 10 def (pure ())

{-|
    Create a new 'TcpServer' with TLS transport.  It forks a new thread to listen given TCP port.
    It calls user provided 'TransportHandler' every time it accepts new TCP connection from peer.
    The 'TransportHandler' is executed by its dedicated thread.
-}
newTlsServer
    :: TcpServerConfig      -- ^ Server configuration
    -> (Context -> IO ())   -- ^ User provided function handling Context.
    -> IO ()                -- ^ newTcpSever returns created server object.
newTlsServer conf handler = newTcpServer conf $ newTlsHandler (tcpServerConfigTlsParams conf) handler
  where
    newTlsHandler :: ServerParams -> (Context -> IO ()) -> (Socket -> IO ())
    newTlsHandler params tlsHandler peer =
        bracket
            (do
                ctx <- contextNew peer params
                handshake ctx
                pure ctx)
            (\ctx ->
                -- If an async exception was thrown, say killThread, we still have active TLS context.
                -- So we need to try to gracefully shutdown it.
                -- However, there is some possibility that the context was already invalidated when we reached here.
                -- For example, if user provided handler returned because it detected EoF on receiving data,
                -- the context can be already invalid and close notification may result error.
                -- Causing IOException by trying to close already invalid context is not a problem here,
                -- so we just catch the exception then ignore it.
                bye ctx `catch` \(_ :: IOException) -> pure ()
            )
            tlsHandler

{-|
    Create a new 'TcpServer' with non-secure transport.  It forks a new thread to listen given TCP port.
    It calls user provided 'TransportHandler' every time it accepts new TCP connection from peer.
    The 'TransportHandler' is executed by its dedicated thread.
-}
newTcpServer
    :: TcpServerConfig      -- ^ Server configuration
    -> (Socket -> IO ())    -- ^ User provided function handling Context.
    -> IO ()                -- ^ newTcpSever returns created server object.
newTcpServer conf@(TcpServerConfig port backlog numWorkers _ readyToConnect) handler =
    bracket newListener close $ \sk -> do
        readyToConnect
        svQ <- newMessageQueue
        makeTcpServer svQ sk
  where
    makeTcpServer svQ sk = do
        workerSVQ <- newMessageQueue
        let workerSVProc    = newProcessSpec [] Permanent $ newSimpleOneForOneSupervisor workerSVQ
            poolKeeperProc  = newProcessSpec [] Permanent $ poolKeeper sk handler workerSVQ numWorkers
        newSupervisor svQ OneForAll def [workerSVProc, poolKeeperProc]

    newListener = do
        sk <- socket AF_INET Stream defaultProtocol
        setSocketOption sk ReuseAddr 1
        bind sk (SockAddrInet port iNADDR_ANY)
        listen sk backlog
        pure sk

type PoolKeeperQueue = MessageQueue ExitReason

poolKeeper
    :: Socket               -- ^ Listening socket.
    -> (Socket -> IO ())    -- ^ Handler of single established socket.
    -> SupervisorQueue      -- ^ Supervisor of workers
    -> Int                  -- ^ Number of worker threads
    -> IO ()
poolKeeper sk handler sv numWorkers = newMessageQueue >>= startPoolKeeper
  where
    startPoolKeeper poolQ = do
        -- TODO not to discard result but escalate error on newChild.
        replicateM_ numWorkers $ void $ newChild def sv worker
        forever $ receive poolQ >>= msgHandler
      where
        msgHandler  Normal  = void $ newChild def sv worker
        msgHandler  Killed  = pure () -- SV is killing workers.  Do nothing more.
        msgHand     _       = putStrLn "uncaught exception" $> () -- TODO handle error properly

        worker              = newProcessSpec [monitor] Temporary $ bracket (fst <$> accept sk) close handler
        monitor reason _    = sendMessage poolQ reason
