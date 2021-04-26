{- |
This module provides an (optionally) persistent store for the state of the IC.

It provides:
 * atomic, blocking, exclusive write access
 * read access to the latest state, at any time

If not backed by a file, it is simply kept in memory.

If backed by a file, state changes are serialized by a separate thread.  It
waits for a state change to happen, then waits for 10s, and then serializes the
then youngest state. If the process is shut down (via `conclude`), it
serializes immediately.

Serialization is atomic (write to a temporary file, then move), so it _should_
be safe to kill the process; _a_ recent state will be persisted.

-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
module IC.StateFile (Store, withStore, modifyStore, peekStore) where

import Codec.Serialise
import qualified Data.ByteString.Lazy as BS
import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.IORef
import System.AtomicWrite.Writer.LazyByteString.Binary
import System.Directory
import System.Mem.StableName
import System.IO
import System.Exit
import Codec.Compression.GZip
import Codec.Compression.Zlib.Internal (DecompressError)

data Store a = Store
    { stateVar :: MVar a
    , lastState :: IORef a
    , pingStorage  :: a -> IO ()
    , conclude :: IO ()
    }

withStore :: Serialise a => IO a -> Maybe FilePath -> (Store a -> IO b) -> IO b
withStore initial backingFile = bracket (newStore initial backingFile) conclude

newStore :: Serialise a => IO a -> Maybe FilePath -> IO (Store a)
newStore initial backingFile = do
    s <- case backingFile of
      Just fn -> do
        ex <- doesFileExist fn
        if ex
        then do
          let err e = do
                hPutStr stderr $ "Error: Couldn't open " ++ fn ++ ":\n"
                hPutStr stderr $ e ++ "\n"
                hPutStr stderr "Maybe the file is corrupt or incompatiblen\n"
                hPutStr stderr $ "Delete " ++ fn ++ " and try again.\n"
                exitFailure
          withFile fn ReadMode $ \hnd -> do
            input <- BS.hGetContents hnd
            case deserialiseOrFail (decompress input) of
              Left  err -> throwIO err
              Right x   -> return x
           `catches`
             [ Handler $ \ex -> err (show (ex :: IOException))
             , Handler $ \ex -> err (show (ex :: DecompressError))
             , Handler $ \ex -> err (show (ex :: DeserialiseFailure))
             ]
        else initial
      Nothing -> initial
    stateVar <- newMVar s
    lastState <- newIORef s
    (pingStorage, conclude) <- case backingFile of
        Nothing -> pure (\_ -> return (), return ())
        Just fn -> writerThread $ \s -> atomicWriteFile fn (compress (serialise s))
    return $ Store { stateVar, lastState, pingStorage, conclude }

modifyStore :: Serialise a => Store a -> StateT a IO b -> IO b
modifyStore store action =
    modifyMVar (stateVar store) $ \(!s) -> do
        n1 <- makeStableName s
        (x, s') <- runStateT action s
        n2 <- makeStableName s'
        -- If the stable names are the same, this means
        -- that the state is unchanged. No need to write new state
        unless (n1 == n2) $ do
            -- Remember last state (for peekStore)
            writeIORef (lastState store) s'
            -- Tell serializer about this
            pingStorage store s'
        return (s', x)

peekStore :: Store a -> IO a
peekStore Store{lastState} = readIORef lastState


-- The abstract logic of the separte writer thread.
--
-- I do not claim to be satisfied with this implementation, surely there must
-- be an easier way to get something that
--
--  * waits 10s after a change
--  * always writes the latest version
--  * when shutting down, interrupts that 10s wait, and waits for the writing
--    to happen.
--
-- So within this function signature and specification, rewrites are welcome.
--
-- Also see https://www.reddit.com/r/haskell/comments/ky1llf/concurrent_programming_puzzle_debouncing_events/
-- which didn’t turn up anything decisively better
--
writerThread ::
  (a -> IO ()) -> -- ^ how to write the thing
  IO (a -> IO (), IO ()) -- notify about new value (async); shutdown (sync)
writerThread doSomething = do
  latest_unwritten <- newEmptyMVar
  write_soon <- newEmptyMVar
  write_now <- newEmptyMVar
  please_shut_down <- newIORef False

  has_shut_down <- newEmptyMVar

  -- A lock for exclusive access to latest_unwritten
  lock <- newMVar ()
  let atomically act = withMVar lock (const act)

  -- the debouncer of the todo notification
  debouncer <- forkIO $ forever $ do
      takeMVar write_soon
      threadDelay (10 * 1000 * 1000) -- 10s
      void $ tryTakeMVar write_soon
      tryPutMVar write_now ()

  -- the writer thread
  void $ forkIO $ fix $ \loop -> do
      takeMVar write_now
      shutdown <- readIORef please_shut_down
      value <- atomically $ tryTakeMVar latest_unwritten
      mapM_ doSomething value
      if shutdown
      then putMVar has_shut_down ()
      else loop

  let notify x = do
        atomically $ do
          void $ tryTakeMVar latest_unwritten
          putMVar latest_unwritten x
        void $ tryPutMVar write_soon ()

  let shutdown = do
        killThread debouncer
        writeIORef please_shut_down True
        void $ tryPutMVar write_now ()
        takeMVar has_shut_down

  return (notify, shutdown)
