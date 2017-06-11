{-# LANGUAGE OverloadedStrings #-}

module Network.TcpServerSpec where

import           Prelude                   hiding (null)

import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forM_, unless, zipWithM_)
import           Data.ByteString           (null)
import qualified Data.ByteString.Char8     as BC8 (pack)
import           Network.Socket            (Family (..), SockAddr (..), Socket,
                                            SocketType (..), close, connect,
                                            defaultProtocol, isConnected,
                                            isWritable, socket,
                                            tupleToHostAddress)
import           Network.Socket.ByteString (recv, sendAll)

import           Test.Hspec

import           Network.TcpServer

listenPort = 8080

helloWorldMessage = "hello, world"

connToServer :: IO Socket
connToServer = do
            sk <- socket AF_INET Stream defaultProtocol
            connect sk $ SockAddrInet listenPort $ tupleToHostAddress (127,0,0,1)
            return sk

startServer :: (ThreadMap -> Socket -> IO ()) -> IO TcpServer
startServer handler = do
    svr <- newServer listenPort handler
    waitListen svr
    return svr


helloServerHandler :: ThreadMap -> Socket -> IO ()
helloServerHandler _ peer = sendAll peer helloWorldMessage

delayedHelloServerHandler :: ThreadMap -> Socket -> IO ()
delayedHelloServerHandler _ peer = do
    threadDelay (10 * 10^6)
    sendAll peer helloWorldMessage

echoServerHandler :: ThreadMap -> Socket -> IO ()
echoServerHandler _ peer = go
  where
    go = do
        msg <- recv peer 4096
        unless (null msg) $ do
            sendAll peer msg
            go

spec :: Spec
spec = do
    describe "a TcpServer with single shot return message" $ do
        it "accepts connection from client" $ do
            svr <- startServer helloServerHandler
            sk <- connToServer
            isConnected sk >>= (`shouldBe` True)
            isWritable sk >>= (`shouldBe` True)
            close sk
            shutdownServer svr

        it "closes sending end after send a message" $ do
            svr <- startServer helloServerHandler
            sk <- connToServer
            isConnected sk >>= (`shouldBe` True)
            isWritable sk >>= (`shouldBe` True)
            msg1 <- recv sk 4096
            msg1 `shouldBe` helloWorldMessage
            msg2 <- recv sk 4096
            null msg2 `shouldBe` True
            close sk
            shutdownServer svr

        it "accepts multiple connection sequencially" $ do
            svr <- startServer helloServerHandler
            sk1 <- connToServer
            isConnected sk1 >>= (`shouldBe` True)
            isWritable sk1 >>= (`shouldBe` True)
            close sk1
            sk2 <- connToServer
            isConnected sk2 >>= (`shouldBe` True)
            isWritable sk2 >>= (`shouldBe` True)
            close sk2
            shutdownServer svr

        it "accepts multiple connection concurrently" $ do
            svr <- startServer helloServerHandler
            sk1 <- connToServer
            sk2 <- connToServer
            isConnected sk1 >>= (`shouldBe` True)
            isWritable sk1 >>= (`shouldBe` True)
            isConnected sk2 >>= (`shouldBe` True)
            isWritable sk2 >>= (`shouldBe` True)
            close sk1
            close sk2
            shutdownServer svr

    describe "TcpServer with delayed single shot return message" $ do
        it "forces disconnecting on server shutdown though handler has pending job" $ do
            svr <- startServer delayedHelloServerHandler
            sk <- connToServer
            threadDelay (10 * 10^3)
            shutdownServer svr
            threadDelay (10 * 10^3)
            msg1 <- recv sk 4096
            null msg1 `shouldBe` True
            close sk

    describe "EchoServer" $ do
        it "receives a message and echo back it" $ do
            svr <- startServer echoServerHandler
            sk <- connToServer
            sendAll sk "hello"
            msg <- recv sk 4096
            msg `shouldBe` "hello"
            close sk
            shutdownServer svr

        it "echoes messages in arriving order" $ do
            svr <- startServer echoServerHandler
            sk <- connToServer
            sendAll sk "hello, "
            sendAll sk "world"
            threadDelay (100 * 10^3)
            msg <- recv sk 4096
            msg `shouldBe` "hello, world"
            close sk
            shutdownServer svr

        it "receives and echoes messages in each session indipendently" $ do
            svr <- startServer echoServerHandler
            sk1 <- connToServer
            sk2 <- connToServer
            sendAll sk1 "hello"
            sendAll sk2 "world"
            msg1 <- recv sk1 4096
            msg2 <- recv sk2 4096
            msg1 `shouldBe` "hello"
            msg2 `shouldBe` "world"
            close sk1
            close sk2
            shutdownServer svr

        it "handles many sequencial sessions" $ do
            svr <- startServer echoServerHandler
            forM_ [1..100] $ \n -> do
                sk <- connToServer
                let smsg = BC8.pack $ show n
                sendAll sk smsg
                rmsg <- recv sk 4096
                rmsg `shouldBe` smsg
                close sk
            shutdownServer svr

        it "handles many concurrent sessions" $ do
            svr <- startServer echoServerHandler
            let smsgs = map (BC8.pack . show) [1..100]
            sks <- mapM (const connToServer) [1..100]
            zipWithM_ sendAll sks smsgs
            rmsgs <- mapM (`recv` 4096) sks
            smsgs `shouldBe` rmsgs
            mapM_ close sks
            print "showdown"
            shutdownServer svr
