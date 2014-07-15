module Main
    (
      main
    ) where

import           Control.Concurrent
import qualified Control.Exception         as E
import           Control.Monad             (forever)
import           Data.ByteString           (hGetLine)
import           Data.ByteString.Char8     (hPutStrLn)
import           Network                   (withSocketsDo)
import           Network.Socket
import qualified Network.Socket.ByteString as N
import           System.IO                 (BufferMode (LineBuffering), Handle,
                                            hClose, hSetBuffering)
import           Text.Printf               (printf)

port :: Int
port = 8080

main :: IO ()
main = withSocketsDo $ do
    mv <- newEmptyMVar
    E.bracket (forkFinally mainThread (const $ putMVar mv ()))
              killThread
              (const $ takeMVar mv)
  where
    mainThread = do
        sock <- socket AF_INET Stream defaultProtocol
        (ainfo:_) <- getAddrInfo hints (Just "127.0.0.1") (Just $ show port)
        let addr = addrAddress ainfo
        bind sock addr
        listen sock 2048
        printf "Listening on port %d\n" port
        let loop = do (csock, _) <- accept sock
                      forkFinally (echo csock) (const $ sClose csock)
                      loop
        loop

    hints = Just $ defaultHints {addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]}


echo :: Socket -> IO ()
echo sock = do
    loop
  where
    loop = do
        line <- N.recv sock 128
        N.sendAll sock line
        loop
