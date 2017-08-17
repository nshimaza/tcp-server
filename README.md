# tcp-server

[![Build Status](https://travis-ci.org/nshimaza/tcp-server.svg?branch=master)](https://travis-ci.org/nshimaza/tcp-server)

A Simple Plain TCP Server.

Boot a simple, plain, general purpose TCP server with automatic hierarchical thread management.
It accepts a user provided handler and forks dedicated thread which listens given TCP port number.
Every time the listening thread accept a new connection from peer, it forks new thread dedicated for
each connection then calls user provided handler.

When server shutdown, all threads including listening thread, handler threads and their children
automatically killed.  You don't have to close the socket by your own.  This module automatically
close the socket on handler exit regardless if it exited normally or it was killed.
You can install your own cleanup routine using finally or bracket.


### Example

```haskell
import           Control.Monad        (forever, unless)
import qualified Data.ByteString      as B (null)
import qualified Data.ByteString.Lazy as BL (fromStrict)

import           Network.TcpServer

newEchoServer :: IO TcpServer
newEchoServer = newTlsServer 8443 echoServerHandler

echoServerHandler :: TransportHandler IO
echoServerHandler _ peer = go
  where
    go = do
        msg <- transportRecv peer
        unless (B.null msg) $ do
            transportSend peer $ BL.fromStrict msg
            go
```
