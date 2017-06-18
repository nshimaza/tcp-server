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
```
