{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    (
      main
    ) where

import           Control.Concurrent
import qualified Control.Exception         as E
import           Control.Monad             (replicateM, void)
import           Data.ByteString.Char8     ()
import           Data.IORef                (IORef, atomicModifyIORef', newIORef,
                                            readIORef)
import           Network.Socket
import qualified Network.Socket.ByteString as N
import           System.Environment        (getArgs)

main :: IO ()
main = do
    [ip, port, spawnCount, pingFreq] <- getArgs
    count <- newIORef (0 :: Int)
    go ip (read port) (read spawnCount) (read pingFreq) count
  where
    go ip port spawnCount pingFreq countRef = E.bracket start end wait
      where
        start = do
            w_tid   <- forkIO $ watcher countRef
            clients <- replicateM spawnCount spawnClient
            return (w_tid, clients)

        end (t, xs) = eatExceptions (killThread t) >> mapM_ killClient xs

        wait = mapM_ waitClient . snd

        spawnClient = E.mask $ \restore -> do
            mv <- newEmptyMVar
            tid <- restore (echoClient ip port pingFreq countRef) `forkFinally` const (putMVar mv ())
            return (tid, mv)

        killClient = eatExceptions . killThread . fst

        waitClient = takeMVar . snd

        eatExceptions m = m `E.catch` \(_ :: E.SomeException) -> return ()


echoClient :: String -> Int -> Float -> IORef Int -> IO ()
echoClient host port pingFreq count = do
    sock <- socket AF_INET Stream defaultProtocol
    (ainfo:_) <- getAddrInfo hints (Just host) (Just $ show port)
    let addr = addrAddress ainfo
    connect sock addr
    incRef count
    E.finally (loop sock) (decRef count)
  where
    hints = Just $ defaultHints {addrFlags = [AI_NUMERICSERV]}
    loop sock = let go = do N.sendAll sock "PING\n"
                            _ <- N.recv sock 128
                            threadDelay (round $ pingFreq*1000000)
                            go
                in go


watcher :: IORef Int -> IO ()
watcher i = go
  where
    go = do
        count <- readIORef i
        putStrLn $ "Clients Connected: " ++ (show count)
        threadDelay (5*1000000)
        go


incRef :: Num a => IORef a -> IO ()
incRef ref = void $ atomicModifyIORef' ref (\x -> (x+1, ()))


decRef :: Num a => IORef a -> IO ()
decRef ref = void $ atomicModifyIORef' ref (\x -> (x-1, ()))
