{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Network.TcpServerSpec where

import           Prelude                    hiding (null)

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (unless, (>=>))
import           Data.ByteString            (null)
import qualified Data.ByteString.Char8      as BC8 (pack)
import           Data.ByteString.Lazy       (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BLC8 (pack)
import           Data.Default.Class
import           Data.Foldable              (for_)
import           Data.Functor               (($>))
import           Data.Maybe                 (isJust)
import           Data.Monoid                ((<>))
import           Data.Traversable           (for)
import           Network.Socket             (Family (..), PortNumber,
                                             ShutdownCmd (..), SockAddr (..),
                                             Socket, SocketType (..), close,
                                             connect, defaultProtocol,
                                             gracefulClose, socket,
                                             tupleToHostAddress)
import qualified Network.Socket.ByteString  as C (recv, sendAll)
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
import           UnliftIO

import           Test.Hspec

import           Network.TcpServer

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

helloWorldMessage = "hello, world"

withTcpServer :: TcpServerConfig -> (Socket -> IO ()) -> (PortNumber -> IO ()) -> IO ()
withTcpServer userConf handler inner = do
    readyMarker <- newEmptyMVar
    let conf = userConf { tcpServerConfigBeforeMainLoop = putMVar readyMarker }
    withAsync (newTcpServer conf handler) $ \_ -> takeMVar readyMarker >>= inner

connToTcpServer :: PortNumber -> IO Socket
connToTcpServer port = do
            sk <- socket AF_INET Stream defaultProtocol
            connect sk . SockAddrInet port $ tupleToHostAddress (127,0,0,1)
            pure sk

withTcpConnection :: PortNumber -> (Socket -> IO ()) -> IO ()
withTcpConnection port inner = bracket (connToTcpServer port) close inner

loggingHooks = def { loggingPacketSent = \packet -> putStrLn ("C: PacketSent " <> show packet)
                   , loggingPacketRecv = \packet -> putStrLn ("C: PacketRecv " <> show packet)
                   , loggingIOSent = \io -> putStrLn ("C: IOSent " <> show io)
                   , loggingIORecv = \header io -> putStrLn ("C: IORecv header:" <> show header <> " io:" <> show io)
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


withTlsServer :: TcpServerConfig -> (Context -> IO ()) -> (PortNumber -> IO ()) -> IO ()
withTlsServer userConf handler inner = do
    readyMarker <- newEmptyMVar
    let conf = userConf { tcpServerConfigTlsParams      = serverParams
                        , tcpServerConfigBeforeMainLoop = putMVar readyMarker }
    withAsync (newTlsServer conf handler) $ \_ -> takeMVar readyMarker >>= inner

noServerValidation = ValidationCache (\_ _ _ -> pure ValidationCachePass) (\_ _ _ -> pure ())

clientParams port = ClientParams { clientUseMaxFragmentLength = Nothing
                                 , clientServerIdentification = ("127.0.0.1", (BC8.pack . show) port)
                                 , clientUseServerNameIndication = False
                                 , clientWantSessionResume = Nothing
                                 , clientShared = def { sharedValidationCache = noServerValidation }
                                 , clientHooks = def
                                 , clientSupported = def { supportedCiphers = ciphersuite_default }
                                 , clientDebug = def
                                 , clientEarlyData = Nothing
                                 }

withTlsConnection :: PortNumber -> (Context -> IO ()) -> IO ()
withTlsConnection port inner = withTcpConnection port $ \sk -> do
    ctx <- contextNew sk $ clientParams port
    handshake ctx
    inner ctx
    bye ctx `catchIO` \_ -> pure ()

helloServerHandler :: Transport t => t -> IO ()
helloServerHandler peer = send peer helloWorldMessage

delayedHelloServerHandler :: Transport t => t -> IO ()
delayedHelloServerHandler peer = do
    threadDelay (10 * 10^6)
    send peer helloWorldMessage

echoServerHandler :: Transport t => t -> IO ()
echoServerHandler peer = go
  where
    go = do
        msg <- recv peer
        unless (null msg) $ do
            send peer $ fromStrict msg
            go

spec :: Spec
spec = do
    describe "TCP based TcpServer with single shot return message" $ do

        it "accepts connection from client, closes sending end after send a message" $
            withTcpServer def helloServerHandler $ \port -> withTcpConnection port $ \peer -> do
                msg1 <- C.recv peer 4096
                fromStrict msg1 `shouldBe` helloWorldMessage
                msg2 <- C.recv peer 4096
                null msg2 `shouldBe` True

        it "accepts multiple connection sequentially" $
            withTcpServer def helloServerHandler $ \port -> do
                withTcpConnection port $ \peer1 -> do
                    msg1 <- C.recv peer1 4096
                    fromStrict msg1 `shouldBe` helloWorldMessage
                    msg2 <- C.recv peer1 4096
                    null msg2 `shouldBe` True
                withTcpConnection port $ \peer2 -> do
                    msg3 <- C.recv peer2 4096
                    fromStrict msg3 `shouldBe` helloWorldMessage
                    msg4 <- C.recv peer2 4096
                    null msg4 `shouldBe` True

        it "accepts multiple connection concurrently" $
            withTcpServer def helloServerHandler $ \port ->
                withTcpConnection port $ \peer1 ->
                    withTcpConnection port $ \peer2 -> do
                        msg1 <- C.recv peer1 4096
                        fromStrict msg1 `shouldBe` helloWorldMessage
                        msg2 <- C.recv peer1 4096
                        null msg2 `shouldBe` True
                        msg3 <- C.recv peer2 4096
                        fromStrict msg3 `shouldBe` helloWorldMessage
                        msg4 <- C.recv peer2 4096
                        null msg4 `shouldBe` True

    describe "TCP based TcpServer with delayed single shot return message" $
        it "forces disconnecting on server shutdown though handler has pending job" $ do
            readyMarker <- newEmptyMVar
            let conf = def { tcpServerConfigBeforeMainLoop = putMVar readyMarker }
            withAsync (newTcpServer conf delayedHelloServerHandler) $ \sv -> do
                takeMVar readyMarker >>= \port -> withTcpConnection port $ \peer -> do
                    cancel sv
                    msg1 <- C.recv peer 4096
                    null msg1 `shouldBe` True

    describe "TCP base EchoServer" $ do
        it "receives a message and echo back it" $
            withTcpServer def echoServerHandler $ \port -> withTcpConnection port $ \peer -> do
                C.sendAll peer "hello"
                msg <- C.recv peer 4096
                msg `shouldBe` "hello"

        it "echoes messages in arriving order" $
            withTcpServer def echoServerHandler $ \port -> withTcpConnection port $ \peer -> do
                C.sendAll peer "hello, "
                C.sendAll peer "world"
                threadDelay (100 * 10^3)
                msg <- C.recv peer 4096
                msg `shouldBe` "hello, world"

        it "receives and echoes messages in each session independently" $
            withTcpServer def echoServerHandler $ \port ->
                withTcpConnection port $ \peer1 ->
                    withTcpConnection port $ \peer2 -> do
                        C.sendAll peer1 "hello"
                        C.sendAll peer2 "world"
                        msg1 <- C.recv peer1 4096
                        msg2 <- C.recv peer2 4096
                        msg1 `shouldBe` "hello"
                        msg2 `shouldBe` "world"

        it "handles many sequential sessions" $
            withTcpServer def echoServerHandler $ \port ->
                for_ [1..100] $ \n -> withTcpConnection port $ \peer -> do
                    let smsg = BC8.pack $ show n
                    C.sendAll peer smsg
                    rmsg <- C.recv peer 4096
                    rmsg `shouldBe` smsg

        it "handles many concurrent sessions" $ do
            let conf = def { tcpServerConfigBacklog = 1000
                           , tcpServerConfigNumWorkers = 1000 }
            withTcpServer conf echoServerHandler $ \port -> do
                synchronizers <- for [1..1000] $ \n -> do
                    threadDelay 1000
                    marker <- newEmptyMVar
                    trigger <- newEmptyMVar
                    async . withTcpConnection port $ \peer -> do
                        let msg = BC8.pack $ show n
                        C.sendAll peer msg
                        rmsg <- C.recv peer 4096
                        rmsg `shouldBe` msg
                        putMVar marker ()
                        takeMVar trigger
                    pure (marker, trigger)
                for_ (map fst synchronizers) readMVar
                for_ (map snd synchronizers) $ \trigger -> putMVar trigger ()

    describe "TLS based TcpServer with single shot return message" $ do
        it "closes sending end after send a message" $
            withTlsServer def helloServerHandler $ \port -> withTlsConnection port $ \ctx -> do
                    msg1 <- recvData ctx
                    fromStrict msg1 `shouldBe` helloWorldMessage
                    msg2 <- recvData ctx
                    null msg2 `shouldBe` True

        it "accepts multiple connection sequentially" $
            withTlsServer def helloServerHandler $ \port -> do
                withTlsConnection port $ contextGetInformation >=> (`shouldSatisfy` isJust)
                withTlsConnection port $ contextGetInformation >=> (`shouldSatisfy` isJust)


        it "accepts multiple connection concurrently" $
            withTlsServer def helloServerHandler $ \port ->
                withTlsConnection port $ \ctx1 ->
                    withTlsConnection port $ \ctx2 -> do
                        contextGetInformation ctx1 >>= (`shouldSatisfy` isJust)
                        contextGetInformation ctx2 >>= (`shouldSatisfy` isJust)

    describe "TLS based TcpServer with delayed single shot return message" $
        it "forces disconnecting on server shutdown though handler has pending job" $ do
            readyMarker <- newEmptyMVar
            let conf = def { tcpServerConfigTlsParams      = serverParams
                           , tcpServerConfigBeforeMainLoop = putMVar readyMarker }
            withAsync (newTlsServer conf delayedHelloServerHandler) $ \sv -> do
                takeMVar readyMarker >>= \port -> withTlsConnection port $ \ctx -> do
                    cancel sv
                    msg1 <- recvData ctx
                    null msg1 `shouldBe` True

    describe "TLS base EchoServer" $ do
        it "receives a message and echo back it" $
            withTlsServer def echoServerHandler $ \port -> withTlsConnection port $ \ctx -> do
                sendData ctx "hello"
                msg <- recvData ctx
                msg `shouldBe` "hello"

        it "echoes messages in arriving order" $
            withTlsServer def echoServerHandler $ \port -> withTlsConnection port $ \ctx -> do
                sendData ctx "hello, "
                sendData ctx "world"
                msg1 <- recvData ctx
                msg2 <- recvData ctx
                msg1 <> msg2 `shouldBe` "hello, world"

        it "receives and echoes messages in each session independently" $
            withTlsServer def echoServerHandler $ \port ->
                withTlsConnection port $ \ctx1 ->
                    withTlsConnection port $ \ctx2 -> do
                        sendData ctx1 "hello"
                        sendData ctx2 "world"
                        msg1 <- recvData ctx1
                        msg2 <- recvData ctx2
                        msg1 `shouldBe` "hello"
                        msg2 `shouldBe` "world"

        it "handles many sequential sessions" $ do
            withTlsServer def echoServerHandler $ \port ->
                for_ [1..100] $ \n -> withTlsConnection port $ \ctx -> do
                    let smsg = BLC8.pack $ show n
                    sendData ctx smsg
                    rmsg <- recvData ctx
                    fromStrict rmsg `shouldBe` smsg

        it "handles many concurrent sessions" $ do
            let conf = def { tcpServerConfigBacklog = 1000
                           , tcpServerConfigNumWorkers = 1000 }
            withTlsServer conf echoServerHandler $ \port -> do
                synchronizers <- for [1..1000] $ \n -> do
                    threadDelay 1000
                    marker <- newEmptyMVar
                    trigger <- newEmptyMVar
                    async . withTlsConnection port $ \ctx -> do
                        let msg = BLC8.pack $ show n
                        sendData ctx msg
                        rmsg <- recvData ctx
                        fromStrict rmsg `shouldBe` msg
                        putMVar marker ()
                        takeMVar trigger
                    pure (marker, trigger)
                for_ (map fst synchronizers) readMVar
                for_ (map snd synchronizers) $ \trigger -> putMVar trigger ()
