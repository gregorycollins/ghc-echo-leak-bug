{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    (
      main
    ) where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8
import           Blaze.ByteString.Builder.Internal.Buffer (allocBuffer)
import           Control.Concurrent
import qualified Control.Exception                        as E
import           Control.Monad                            (replicateM, void)
import           Data.ByteString.Char8                    ()
import           Data.IORef                               (IORef,
                                                           atomicModifyIORef,
                                                           newIORef, readIORef)
import           Data.Monoid                              ((<>))
import           Network.Socket
import qualified Network.Socket.ByteString                as N
import           System.Environment                       (getArgs)
import qualified System.IO.Streams                        as Streams
import qualified System.IO.Streams.Builder                as Streams

atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b


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

        end (t, xs) = E.mask_ $ do
            eatExceptions (killThread t)
            mapM_ killClient xs
            mapM_ waitClient xs

        wait = mapM_ waitClient . snd

        spawnClient = E.mask_ $ do
            mv <- newEmptyMVar
            tid <- forkIOWithUnmask (\rest -> rest (echoClient ip port pingFreq countRef) `E.finally` putMVar mv ())
            return (tid, mv)

        killClient = eatExceptions . killThread . fst

        waitClient = readMVar . snd

        eatExceptions m = m `E.catch` \(_ :: E.SomeException) -> return ()


echoClient :: String -> Int -> Float -> IORef Int -> IO ()
echoClient host port pingFreq count =
    E.bracket (socket AF_INET Stream defaultProtocol)
              (\sock -> do shutdown sock ShutdownBoth
                           sClose sock
                           decRef count)
              (\sock -> do
                  (ainfo:_) <- getAddrInfo hints (Just host) (Just $ show port)
                  let addr = addrAddress ainfo
                  connect sock addr
                  incRef count
                  (is, osB) <- Streams.socketToStreamsWithBufferSize bUFSIZ sock
                  buf <- allocBuffer bUFSIZ
                  os <- Streams.unsafeBuilderStream (return buf) osB
                  loop is os)
  where
    bUFSIZ = 128
    hints = Just $ defaultHints {addrFlags = [AI_NUMERICSERV]}
    msg = Just (fromByteString "PING\n" <> flush)
    loop is os = let go = do Streams.write msg os
                             Streams.read is >>= maybe (return ()) (\x -> do
                                 threadDelay (round $ pingFreq*1000000)
                                 go)
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
