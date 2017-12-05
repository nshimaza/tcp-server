{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TcpServerSpec where

import           Prelude                    hiding (null)

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (IOException, bracket, catch)
import           Control.Monad              (unless, void, zipWithM_)
import           Data.ByteString            (null, singleton)
import qualified Data.ByteString.Char8      as BC8 (pack)
import           Data.ByteString.Lazy       (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BLC8 (pack)
import           Data.Default.Class
import           Data.Foldable              (for_, traverse_)
import           Data.Maybe                 (isJust)
import           Data.Monoid                ((<>))
import           Data.Traversable           (traverse)
import           Network.Socket             (Family (..), ShutdownCmd (..),
                                             SockAddr (..), Socket,
                                             SocketType (..), close, connect,
                                             defaultProtocol, isConnected,
                                             isWritable, shutdown, socket,
                                             tupleToHostAddress)
import           Network.Socket.ByteString  (recv, sendAll)
import           Network.TLS                (ClientParams (..), Context,
                                             Credentials (..), Logging (..),
                                             ServerParams (..), Shared (..),
                                             ValidationCache (..),
                                             ValidationCacheResult (..), bye,
                                             contextClose,
                                             contextGetInformation,
                                             contextHookSetLogging, contextNew,
                                             credentialLoadX509FromMemory,
                                             handshake, recvData, sendData,
                                             sharedCredentials,
                                             supportedCiphers,
                                             supportedClientInitiatedRenegotiation)
import           Network.TLS.Extra.Cipher   (ciphersuite_default)

import           Test.Hspec

import           Network.TcpServer

listenPort = 8080

helloWorldMessage = "hello, world"

startTcpServer :: TransportHandler -> IO TcpServer
startTcpServer handler = do
    svr <- newTcpServer listenPort handler
    waitListen svr
    return svr

connToTcpServer :: IO Socket
connToTcpServer = do
            sk <- socket AF_INET Stream defaultProtocol
            connect sk . SockAddrInet listenPort $ tupleToHostAddress (127,0,0,1)
            return sk

closeTcp :: Socket -> IO ()
closeTcp sk = shutdown sk ShutdownSend `catch` (\(_ :: IOException) -> return ()) >> close sk

loggingHooks = def { loggingPacketSent = \packet -> putStrLn ("C: PacketSent " ++ show packet)
                   , loggingPacketRecv = \packet -> putStrLn ("C: PacketRecv " ++ show packet)
                   , loggingIOSent = \io -> putStrLn ("C: IOSent " ++ show io)
                   , loggingIORecv = \header io -> putStrLn ("C: IORecv header:" ++ show header ++ " io:" ++ show io)
                   }

certificatePem = "-----BEGIN CERTIFICATE-----\n\
                 \MIIEcTCCA1mgAwIBAgIJAIxzVlmJ0tBTMA0GCSqGSIb3DQEBBQUAMIGBMQswCQYD\n\
                 \VQQGEwJKUDEOMAwGA1UECBMFVG9reW8xDjAMBgNVBAcTBVRva3lvMQ4wDAYDVQQK\n\
                 \EwVEdW1teTEOMAwGA1UECxMFRHVtbXkxEDAOBgNVBAMTB2ZvcnRlc3QxIDAeBgkq\n\
                 \hkiG9w0BCQEWEWR1bW15QGV4YW1wbGUuY29tMB4XDTE3MDYyMzEwMzMxNVoXDTE4\n\
                 \MDYyMzEwMzMxNVowgYExCzAJBgNVBAYTAkpQMQ4wDAYDVQQIEwVUb2t5bzEOMAwG\n\
                 \A1UEBxMFVG9reW8xDjAMBgNVBAoTBUR1bW15MQ4wDAYDVQQLEwVEdW1teTEQMA4G\n\
                 \A1UEAxMHZm9ydGVzdDEgMB4GCSqGSIb3DQEJARYRZHVtbXlAZXhhbXBsZS5jb20w\n\
                 \ggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDfX0fAvpJkt9kNN9OmYZU8\n\
                 \M2O9iogVTbSj0nTCd3aID9aJcdfnZECa0fROO/C3Cjbnm80nUHeskuNmA8Q3oc9C\n\
                 \JOXe+21yjR6A9kP7gLqXkTHmWcZhzgiZxLxdUZTL6m7Gc6UTSauwJrfHsKtfYLcM\n\
                 \ux4LmFq2KtXECCwahTz6JkBqBFkcxpf+Gn3ccJPYcpb/OzuytgwJiuQOOduQ2E+2\n\
                 \2HsHxLW7JneovMjCAVJCCrs9+cjp+O2l4U97JIOqFWLjVt+qB/kjEzFnspPgCl+s\n\
                 \fltls4VZX6BQQofk+UihfxEIjGNHTQcBuD098tSqdjk9vlmutre7B2C1aBAQk3MV\n\
                 \AgMBAAGjgekwgeYwHQYDVR0OBBYEFHSA1t0VgSrJC/fs8h6qWtcHX9NkMIG2BgNV\n\
                 \HSMEga4wgauAFHSA1t0VgSrJC/fs8h6qWtcHX9NkoYGHpIGEMIGBMQswCQYDVQQG\n\
                 \EwJKUDEOMAwGA1UECBMFVG9reW8xDjAMBgNVBAcTBVRva3lvMQ4wDAYDVQQKEwVE\n\
                 \dW1teTEOMAwGA1UECxMFRHVtbXkxEDAOBgNVBAMTB2ZvcnRlc3QxIDAeBgkqhkiG\n\
                 \9w0BCQEWEWR1bW15QGV4YW1wbGUuY29tggkAjHNWWYnS0FMwDAYDVR0TBAUwAwEB\n\
                 \/zANBgkqhkiG9w0BAQUFAAOCAQEAqQoEYsAJ1wIECV4jdA/QPPURlPzthGV8I//I\n\
                 \xtkBojoH9r/q0VCTaKhxmEwS3wU8TDuH1SM3CJp1AYCl812OWuo4khBeMTi1ltxl\n\
                 \71yWkGqYQxrqUNvfr4Oy12irv09D4nsSmRPbWHz/RPLFKIiCAyLgBIq5HhO8KzyP\n\
                 \lNyzsUIPs55xdCTLKH+O48gg80RXNRLOk3XTZuNIHKtbZgVE9k4Vo+vW8/SfDdmK\n\
                 \8CqUgXyU05rbMUW5bJjBELoARxW1+K730wo8POhcur2r5XC574RLfCdInWQqFLgd\n\
                 \bnqHotrGDXbBuKQcobzRQTPWZCAuemsI2S5ZxIbhdwu3TH5FJA==\n\
                 \-----END CERTIFICATE-----"

keyPem = "-----BEGIN RSA PRIVATE KEY-----\n\
         \MIIEpQIBAAKCAQEA319HwL6SZLfZDTfTpmGVPDNjvYqIFU20o9J0wnd2iA/WiXHX\n\
         \52RAmtH0Tjvwtwo255vNJ1B3rJLjZgPEN6HPQiTl3vttco0egPZD+4C6l5Ex5lnG\n\
         \Yc4ImcS8XVGUy+puxnOlE0mrsCa3x7CrX2C3DLseC5hatirVxAgsGoU8+iZAagRZ\n\
         \HMaX/hp93HCT2HKW/zs7srYMCYrkDjnbkNhPtth7B8S1uyZ3qLzIwgFSQgq7PfnI\n\
         \6fjtpeFPeySDqhVi41bfqgf5IxMxZ7KT4ApfrH5bZbOFWV+gUEKH5PlIoX8RCIxj\n\
         \R00HAbg9PfLUqnY5Pb5Zrra3uwdgtWgQEJNzFQIDAQABAoIBAGN4CnvKYWZ4eC9q\n\
         \rNVqnwov2DqPdgoGkGpjTZtLl3z/kImkKIDiI+LNm/06s/bOlaSNvM3FB4XR43GK\n\
         \O9aKN8E8rSCe1h4suWi0QZG9wVm9TntFfxIOrcXL1hW4PoqP2sSEuv3b0JigpTZH\n\
         \dgkDCICi0r5XGg4FBSxGd11X989sZCe978mz3sCxZIkADk6IpKqLGOPd3iLuwZS+\n\
         \Pxa72pFv4QqzyQ8T00pgFflc1Szmj8CCBrgO7hnJo37SI+ggOM/GAXoKpwBkqUgD\n\
         \Rlw0sVC8zbXYNu278uGCdghYb14vFvH7P+ZLUXAhq+g9T6BqYp1ktwUCyDjBqJKe\n\
         \UQ9vWAECgYEA96VAHlU9xTW6OdsUZ+F6neAMqVExUFtj5G1mLsVr+m2YUH1UjDat\n\
         \Dc15TQPZ+zHA2rhlu0pOfviYJ3HSD2qwg8zsCt7yIS3rPOqsPRn3TlTNsF1hLSCC\n\
         \rdhleDB0l2tRb2eDHfkzStm/tHJi74wQV4feJJtDGEgKtzVijcDvrKMCgYEA5uhl\n\
         \tRkvxElbcqanMgMeQscvGmPYXVuDT1Eb7AyXIPWCh8gCg+DaPB4ubXwGFvpuZcsJ\n\
         \UrpsV90T2M3ne5ie9+4BsowaC4KHBy8pBRgVMds3I0o8DqBGxCy6m/YTXIWhYDdV\n\
         \Lx24r0ilM1/48uSgPbZfd4RZHu1JSDMMfoqVZOcCgYEAnyu0oYpyUG4vCv64B+jS\n\
         \EeUu3ikUhCypcOmr8sXrmCOW8a0Mia6e3i9CFvExyxV4elxo7s9c0plf2oJ63HPb\n\
         \jmtJD9eqUw8bkqTXHdKxtMQnDIOIcSPjOm/LEEw8tBJDXlJu4PYKNMLuR8H5uzQQ\n\
         \UOJAMtEkd9feI1GJzcjDg/cCgYEAg4sLPfMeQi0kOpl0EEq0d4hvbhJJVU3n7Uyg\n\
         \ooW9ptoWRXIM9MIosSzkeBy6f2alc7oKEqX/SMK6Jr2OTNvyW/2r2+Jyh/IRKGeW\n\
         \b797e46Nx73nntFx3xo4KNczynaaK2z6S0AcUUcKTc0zznOuUdnOYxDzkGnPYKEs\n\
         \Uf5zAysCgYEAggHpi2g0SQi+GZjG9a4BbdDBiEX44V+BPf2LSd7bOvnC2YxGnppv\n\
         \hJcQR8jXO7NAmP/IVdk8X8smdfBGxjoQ8rmjUflYUEZCXbhPQLozc0nx7KWdjPKI\n\
         \sEHtYlvmmE0kZDbvco4caCATvX7pQgcpx6+chObTm6JLtYHoqcVjXPU=\n\
         \-----END RSA PRIVATE KEY-----"

serverCredential = right $ credentialLoadX509FromMemory certificatePem keyPem where
    right (Right cred) = cred
    right (Left err)   = error err

serverParams = def { serverShared = def { sharedCredentials = Credentials [serverCredential] }
                   , serverSupported = def { supportedCiphers = ciphersuite_default
                                           , supportedClientInitiatedRenegotiation = True }
                   }

startTlsServer :: TransportHandler -> IO TcpServer
startTlsServer handler = do
    svr <- newTlsServer serverParams listenPort handler
    waitListen svr
    return svr

noServerValidation = ValidationCache (\_ _ _ -> return ValidationCachePass) (\_ _ _ -> return ())

clientParams = ClientParams { clientUseMaxFragmentLength = Nothing
                            , clientServerIdentification = ("127.0.0.1", (BC8.pack . show) listenPort)
                            , clientUseServerNameIndication =False
                            , clientWantSessionResume = Nothing
                            , clientShared = def { sharedValidationCache = noServerValidation }
                            , clientHooks = def
                            , clientSupported = def { supportedCiphers = ciphersuite_default }
                            , clientDebug = def
                            }

connToTlsServer :: IO (Context, Socket)
connToTlsServer = do
    sk <- connToTcpServer
    ctx <- contextNew sk clientParams
    handshake ctx
    return (ctx, sk)

closeTls :: Context -> Socket -> IO ()
closeTls ctx sk = bye ctx `catch` (\(_ :: IOException) -> return ()) >> closeTcp sk

helloServerHandler :: ThreadMap -> Transport -> IO ()
helloServerHandler _ peer = transportSend peer helloWorldMessage

delayedHelloServerHandler :: ThreadMap -> Transport -> IO ()
delayedHelloServerHandler _ peer = do
    threadDelay (10 * 10^6)
    transportSend peer helloWorldMessage

echoServerHandler :: ThreadMap -> Transport -> IO ()
echoServerHandler _ peer = go
  where
    go = do
        msg <- transportRecv peer
        unless (null msg) $ do
            transportSend peer $ fromStrict msg
            go

spec :: Spec
spec = do
    describe "TCP based TcpServer with single shot return message" $ do
        it "accepts connection from client" $ do
            bracket (startTcpServer helloServerHandler) shutdownServer $ \_ -> do
                sk <- connToTcpServer
                isConnected sk >>= (`shouldBe` True)
                isWritable sk >>= (`shouldBe` True)
                close sk

        it "closes sending end after send a message" $ do
            bracket (startTcpServer helloServerHandler) shutdownServer $ \_ -> do
                sk <- connToTcpServer
                isConnected sk >>= (`shouldBe` True)
                isWritable sk >>= (`shouldBe` True)
                msg1 <- recv sk 4096
                fromStrict msg1 `shouldBe` helloWorldMessage
                msg2 <- recv sk 4096
                null msg2 `shouldBe` True
                close sk

        it "accepts multiple connection sequencially" $ do
            bracket (startTcpServer helloServerHandler) shutdownServer $ \_ -> do
                sk1 <- connToTcpServer
                isConnected sk1 >>= (`shouldBe` True)
                isWritable sk1 >>= (`shouldBe` True)
                close sk1
                sk2 <- connToTcpServer
                isConnected sk2 >>= (`shouldBe` True)
                isWritable sk2 >>= (`shouldBe` True)
                close sk2

        it "accepts multiple connection concurrently" $ do
            bracket (startTcpServer helloServerHandler) shutdownServer $ \_ -> do
                sk1 <- connToTcpServer
                sk2 <- connToTcpServer
                isConnected sk1 >>= (`shouldBe` True)
                isWritable sk1 >>= (`shouldBe` True)
                isConnected sk2 >>= (`shouldBe` True)
                isWritable sk2 >>= (`shouldBe` True)
                close sk1
                close sk2

    describe "TCP based TcpServer with delayed single shot return message" $
        it "forces disconnecting on server shutdown though handler has pending job" $ do
            svr <- startTcpServer delayedHelloServerHandler
            sk <- connToTcpServer
            threadDelay (10 * 10^3)
            shutdownServer svr
            threadDelay (10 * 10^3)
            msg1 <- recv sk 4096
            null msg1 `shouldBe` True
            close sk

    describe "TCP base EchoServer" $ do
        it "receives a message and echo back it" $ do
            bracket (startTcpServer echoServerHandler) shutdownServer $ \_ -> do
                sk <- connToTcpServer
                sendAll sk "hello"
                msg <- recv sk 4096
                msg `shouldBe` "hello"
                close sk

        it "echoes messages in arriving order" $ do
            bracket (startTcpServer echoServerHandler) shutdownServer $ \_ -> do
                sk <- connToTcpServer
                sendAll sk "hello, "
                sendAll sk "world"
                threadDelay (100 * 10^3)
                msg <- recv sk 4096
                msg `shouldBe` "hello, world"
                close sk

        it "receives and echoes messages in each session indipendently" $ do
            bracket (startTcpServer echoServerHandler) shutdownServer $ \_ -> do
                sk1 <- connToTcpServer
                sk2 <- connToTcpServer
                sendAll sk1 "hello"
                sendAll sk2 "world"
                msg1 <- recv sk1 4096
                msg2 <- recv sk2 4096
                msg1 `shouldBe` "hello"
                msg2 `shouldBe` "world"
                close sk1
                close sk2

        it "handles many sequencial sessions" $ do
            bracket (startTcpServer echoServerHandler) shutdownServer $ \_ -> do
                for_ [1..100] $ \n -> do
                    sk <- connToTcpServer
                    let smsg = BC8.pack $ show n
                    sendAll sk smsg
                    rmsg <- recv sk 4096
                    rmsg `shouldBe` smsg
                    close sk

        it "handles many concurrent sessions" $ do
            bracket (startTcpServer echoServerHandler) shutdownServer $ \_ -> do
                let smsgs = map (BC8.pack . show) [1..100]
                sks <- traverse (const connToTcpServer) [1..100]
                zipWithM_ sendAll sks smsgs
                rmsgs <- traverse (`recv` 4096) sks
                smsgs `shouldBe` rmsgs
                traverse_ close sks

    describe "TLS based TcpServer with single shot return message" $ do
        it "closes sending end after send a message" $ do
            bracket (startTlsServer helloServerHandler) shutdownServer $ \_ -> do
                (ctx, sk) <- connToTlsServer
                msg1 <- recvData ctx
                fromStrict msg1 `shouldBe` helloWorldMessage
                msg2 <- recvData ctx
                null msg2 `shouldBe` True
                closeTls ctx sk

        it "accepts multiple connection sequencially" $ do
            bracket (startTlsServer helloServerHandler) shutdownServer $ \_ -> do
                (ctx1, sk1) <- connToTlsServer
                contextGetInformation ctx1 >>= (`shouldSatisfy` isJust)
                closeTls ctx1 sk1
                (ctx2, sk2) <- connToTlsServer
                contextGetInformation ctx2 >>= (`shouldSatisfy` isJust)
                closeTls ctx2 sk2

        it "accepts multiple connection concurrently" $ do
            bracket (startTlsServer helloServerHandler) shutdownServer $ \_ -> do
                (ctx1, sk1) <- connToTlsServer
                (ctx2, sk2) <- connToTlsServer
                contextGetInformation ctx1 >>= (`shouldSatisfy` isJust)
                contextGetInformation ctx2 >>= (`shouldSatisfy` isJust)
                closeTls ctx1 sk1
                closeTls ctx2 sk2

    describe "TLS based TcpServer with delayed single shot return message" $
        it "forces disconnecting on server shutdown though handler has pending job" $ do
            svr <- startTlsServer delayedHelloServerHandler
            (ctx, sk) <- connToTlsServer
            threadDelay (10 * 10^3)
            shutdownServer svr
            threadDelay (10 * 10^3)
            msg1 <- recvData ctx
            null msg1 `shouldBe` True
            closeTls ctx sk

    describe "TLS base EchoServer" $ do
        it "receives a message and echo back it" $ do
            bracket (startTlsServer echoServerHandler) shutdownServer $ \_ -> do
                (ctx, sk) <- connToTlsServer
                sendData ctx "hello"
                msg <- recvData ctx
                msg `shouldBe` "hello"
                closeTls ctx sk

        it "echoes messages in arriving order" $ do
            bracket (startTlsServer echoServerHandler) shutdownServer $ \_ -> do
                (ctx, sk) <- connToTlsServer
                sendData ctx "hello, "
                sendData ctx "world"
                msg1 <- recvData ctx
                msg2 <- recvData ctx
                msg1 <> msg2 `shouldBe` "hello, world"
                closeTls ctx sk

        it "receives and echoes messages in each session indipendently" $ do
            bracket (startTlsServer echoServerHandler) shutdownServer $ \_ -> do
                (ctx1, sk1) <- connToTlsServer
                (ctx2, sk2) <- connToTlsServer
                sendData ctx1 "hello"
                sendData ctx2 "world"
                msg1 <- recvData ctx1
                msg2 <- recvData ctx2
                msg1 `shouldBe` "hello"
                msg2 `shouldBe` "world"
                closeTls ctx1 sk1
                closeTls ctx2 sk2

        it "handles many sequencial sessions" $ do
            bracket (startTlsServer echoServerHandler) shutdownServer $ \_ -> do
                for_ [1..100] $ \n -> do
                    (ctx, sk) <- connToTlsServer
                    let smsg = BLC8.pack $ show n
                    sendData ctx smsg
                    rmsg <- recvData ctx
                    fromStrict rmsg `shouldBe` smsg
                    closeTls ctx sk

        it "handles many concurrent sessions" $ do
            bracket (startTlsServer echoServerHandler) shutdownServer $ \_ -> do
                let smsgs = map (BLC8.pack . show) [1..100]
                ctxsSks <- traverse (const connToTlsServer) [1..100]
                let (ctxs, sks) = unzip ctxsSks
                zipWithM_ sendData ctxs smsgs
                rmsgs <- traverse recvData ctxs
                smsgs `shouldBe` map fromStrict rmsgs
                traverse_ (uncurry closeTls) ctxsSks
