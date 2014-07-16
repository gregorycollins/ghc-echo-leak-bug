module Main
    (
      main
    ) where

import           Control.Concurrent
import qualified Control.Exception         as E
import           Control.Monad             (forever)
import           Data.ByteString           (ByteString)
import           Network                   (withSocketsDo)
import           Network.Socket
import qualified Network.Socket.ByteString as N
import           System.Exit
import           System.IO                 (BufferMode (LineBuffering), Handle,
                                            hClose, hSetBuffering)
import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams         as Streams
import           Text.Printf               (printf)

port :: Int
port = 8080

main :: IO ()
main = withSocketsDo $ do
    mv <- newEmptyMVar
    runInUnboundThread mainThread
  where
    mainThread =
        E.bracket (socket AF_INET Stream 0)
                  (\sock -> do putStrLn "exiting..."
                               sClose sock)
                  (\sock -> do
                      setSocketOption sock ReuseAddr 1
                      setSocketOption sock NoDelay 1
                      (ainfo:_) <- getAddrInfo hints (Just "127.0.0.1") (Just $ show port)
                      let addr = addrAddress ainfo
                      bind sock addr
                      listen sock 2048
                      printf "Listening on port %d\n" port
                      let loop = do
                                    (csock, _) <- accept sock
                                    (is, os) <- Streams.socketToStreamsWithBufferSize 128 csock
                                    forkIOWithUnmask (\r -> r (echo is os) `E.finally` sClose csock)
                                    loop
                      loop)

    hints = Just $ defaultHints {addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]}


echo :: InputStream ByteString -> OutputStream ByteString -> IO ()
echo is os = loop
  where
    loop = Streams.read is >>=
           maybe (return ()) (\ping -> Streams.write (Just ping) os >> loop)
