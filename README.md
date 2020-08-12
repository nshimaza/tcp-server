# tcp-server

[![Build Status](https://github.com/nshimaza/tcp-server/workflows/build/badge.svg)](https://github.com/nshimaza/tcp-server/actions?query=workflow%3Abuild)

A Simple Plain TCP Server.

Create a simple, plain, general purpose TCP server with guaranteed thread
cleanup on exit.  It accepts a user provided handler, create worker thread pool
for incoming TCP connection, a thread managing the pool and their supervisors.
Dedicated thread is assigned to each incoming connection so that user provided
handler need to take care of only single TCP session.

User provided handler is called with accepted socket (or established TLS context
in that the server is TLS server) and automatically closed when the handler is
exit regardless in any reason (normal exit, uncaught exception or killed by
asynchronous exception).

When server is killed, all threads including listening thread and worker threads
calling handler are automatically killed.  User provided handler must ensure
cleaning up itself on such asynchronous termination.


### Example

```haskell
import           Control.Monad        (unless)
import qualified Data.ByteString      as S (null)
import           Data.ByteString.Lazy (fromStrict)
import           Data.Default.Class

import           Network.TcpServer
import           Test.Hspec

conf = def
    { tcpServerConfigPort           = 9000
    , tcpServerConfigBacklog        = 1000
    , tcpServerConfigNumWorkers     = 10000
    }

newEchoServer :: IO ()
newEchoServer = newTcpServer conf echoServerHandler

echoServerHandler :: Transport t => t -> IO ()
echoServerHandler peer = go
  where
    go = do
        msg <- recv peer
        unless (S.null msg) $ do
            send peer $ fromStrict msg
            go
```
