# tcp-server

[![Build Status](https://travis-ci.org/nshimaza/tcp-server.svg?branch=master)](https://travis-ci.org/nshimaza/tcp-server)

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


### Example

```haskell
import           Control.Monad        (forever, unless)
import           Data.ByteString      (null)
import           Data.ByteString.Lazy (fromStrict)

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
        unless (null msg) $ do
            send peer $ fromStrict msg
            go
```
