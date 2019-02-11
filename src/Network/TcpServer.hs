{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Network.TcpServer
Copyright   : (c) Naoto Shimazaki 2017,2018
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

A Simple Plain TCP Server.

Create a simple, plain, general purpose TCP server with guaranteed thread cleanup on exit.  It accepts a user provided
handler, create worker thread pool for incoming TCP connection, a thread managing the pool and their supervisors.
Dedicated thread is assigned to each incoming connection so that user provided handler need to take care of only single
TCP session.

User provided handler is called with accepted socket (or established TLS context in that the server is TLS server) and
automatically closed when the handler is exit regardless in any reason (normal exit, uncaught exception or killed by
asynchronous exception).

When server is killed, all threads including listening thread and worker threads calling handler are automatically
killed.  User provided handler must ensure cleaning up itself on such asynchronous termination.


=== Example

@
import           Control.Monad        (forever, unless)
import qualified Data.ByteString      as B (null)
import qualified Data.ByteString.Lazy as BL (fromStrict)

import           Network.TcpServer

conf = def
    { tcpServerConfigPort           = 8443
    , tcpServerConfigBacklog        = 1000
    , tcpServerConfigNumWorkers     = 10000
    }

newEchoServer :: IO TcpServer
newEchoServer = newTlsServer conf echoServerHandler

echoServerHandler :: Transport t => t -> IO ()
echoServerHandler peer = go
  where
    go = do
        msg <- recv peer
        unless (B.null msg) $ do
            send peer $ BL.fromStrict msg
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
                                                 close, defaultProtocol, listen,
                                                 setSocketOption, shutdown,
                                                 socket)
import qualified Network.Socket.ByteString      (recv)
import qualified Network.Socket.ByteString.Lazy (sendAll)
import           Network.TLS                    (Context, ServerParams (..),
                                                 bye, contextNew, handshake,
                                                 recvData, sendData)
import           UnliftIO

import           Control.Concurrent.Supervisor  hiding (send)
import qualified Control.Concurrent.Supervisor  as SV (send)

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
        makeTcpServer sk
  where
    makeTcpServer sk = do
        (workerSVQ, workerSV) <- newActor newSimpleOneForOneSupervisor
        let workerSVProc    = newProcessSpec Permanent $ noWatch workerSV
            poolKeeperProc  = newProcessSpec Permanent $ noWatch $ poolKeeper sk handler workerSVQ numWorkers
        snd =<< newActor (newSupervisor OneForAll def [workerSVProc, poolKeeperProc])

    newListener = do
        sk <- socket AF_INET Stream defaultProtocol
        setSocketOption sk ReuseAddr 1
        bind sk (SockAddrInet port 0)
        listen sk backlog
        pure sk

-- type PoolKeeperQueue = MessageQueue ExitReason

poolKeeper
    :: Socket               -- ^ Listening socket.
    -> (Socket -> IO ())    -- ^ Handler of single established socket.
    -> SupervisorQueue      -- ^ Supervisor of workers
    -> Int                  -- ^ Number of worker threads
    -> IO ()
poolKeeper sk handler sv numWorkers = newActor startPoolKeeper >>= snd
  where
    startPoolKeeper inbox = do
        -- TODO not to discard result but escalate error on newChild.
        replicateM_ numWorkers $ void $ newChild def sv worker
        forever $ receive inbox >>= msgHandler
      where
        msgHandler  Normal = void $ newChild def sv worker
        msgHandler  Killed = pure () -- SV is killing workers.  Do nothing more.
        msgHandler  _      = putStrLn "uncaught exception" $> () -- TODO handle error properly

        worker              = newProcessSpec Temporary $ watch monitor $ bracket (fst <$> accept sk) close handler
        monitor reason _    = SV.send (Actor inbox) reason
