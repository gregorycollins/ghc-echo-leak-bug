{-# LANGUAGE ScopedTypeVariables #-}

module Main
    (
      main
    ) where

import           Control.Concurrent
import qualified Control.Exception         as E
import           Control.Monad             (forever, void)
import           Data.ByteString           (hGetLine)
import           Data.ByteString.Char8     (hPutStrLn)
import           Network                   (withSocketsDo)
import           Network.Socket
import qualified Network.Socket.ByteString as N
import           System.Exit
import           System.IO                 (BufferMode (LineBuffering), Handle,
                                            hClose, hSetBuffering)
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
                                    forkIOWithUnmask (\r -> r (echo csock) `E.finally` sClose csock)
                                    loop
                      loop)

    hints = Just $ defaultHints {addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]}


eatExceptions :: IO a -> IO ()
eatExceptions m = void m `E.catch` \(_ :: E.SomeException) -> return ()


echo :: Socket -> IO ()
echo sock = eatExceptions loop
  where
    loop = do
        line <- N.recv sock 128
        N.sendAll sock line
        loop
