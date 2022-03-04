{-# LANGUAGE NumericUnderscores #-}
-- TODO write a test for this: https://ro-che.info/articles/2014-07-30-bracket#bracket-in-non-main-threads
-- | Deal with ctrl c events nicely.
--   don't just kill the main thread, kill every other registered thread as well.
-- https://ro-che.info/articles/2014-07-30-bracket
--
-- bitch: https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/
-- bitch II: https://simonmar.github.io/posts/2017-01-24-asynchronous-exceptions.html
module CtrlC
  ( withKillThese
  , CtrlCSettings(..)
  , defSettings
  , forkTracked
  , track
  , untrack
  , installSignalHandlers
  , SignalException(..)
  , printLogger
  , defLogger
  )
where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Concurrent.STM.TChan
import Data.Foldable
import Control.Exception
import Data.Map(Map)
import Data.Set(Set)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import System.Timeout
import Data.Typeable (Typeable)
import System.Posix.Signals
import System.Mem.Weak (deRefWeak)
import Control.Concurrent.Async(Async,async, wait, asyncThreadId, cancel, waitCatch)
import Control.Concurrent.STM.TQueue
import Data.Monoid (Endo(..), appEndo)

data LogMsg = ExitedGracefully
            | TimeOut
            | Killing ThreadId
            | Tracking ThreadId
            | Untracking ThreadId
            | KillingSet (Set ThreadId)
            | WaitingFor (Set ThreadId)
            | StartedKilling

defLogger :: LogMsg -> IO ()
defLogger _ = pure ()

toString :: LogMsg -> String
toString ExitedGracefully = "CtrlC: ExitedGracefully"
toString TimeOut = "CtrlC: TimeOut"
toString (Killing tid) = "CtrlC: Killing " <> show tid
toString (Tracking tid) = "CtrlC: Tracking" <> show tid
toString (Untracking tid) = "CtrlC: Untracking" <> show tid
toString (KillingSet tset) = "CtrlC: Killing set " <> show tset
toString StartedKilling = "CtrlC: Started Killing"
toString (WaitingFor tset) = "CtrlC: Waiting for these threads to untrack themselves to indicate dying gracefully " <> show tset

printLogger :: LogMsg -> IO ()
printLogger = putStrLn . toString

data CtrlCState = MkCtrlCState {
  -- yup this isn't great, I guess v2 would have another worker thread blocking on the queue
  -- and doing the modification of cstate
    ccsTrackedThreads :: TVar (Set ThreadId) -- ^ excluding the main thread
  , ccsSettings :: CtrlCSettings
}
data CtrlCSettings = MkCtrlCSettings {
    csTimeout :: Int -- ^ in microseconds (1/10^6 seconds), prevents infinite blocking, smaller then 0 means no timeout
  , csLogger :: LogMsg -> IO ()
  }

defSettings :: CtrlCSettings
defSettings = MkCtrlCSettings 2000000 defLogger

-- | this will fork out a thread that is already tracked by CtrlCState,
--   it also has the untrack handler attached.
forkTracked :: CtrlCState -> IO () -> IO ThreadId
forkTracked state io =
    forkIO $ do
      tid <- myThreadId
      info $ Tracking tid
      atomically $ track state tid
      io `finally` do
        info $ Untracking tid
        atomically (untrack state tid)
  where
    info :: LogMsg -> IO ()
    info = csLogger (ccsSettings state)

-- | Starts tracking a thread, we expect this thread to untrack itself
--   to indicate it is done with cleaning up.
track :: CtrlCState -> ThreadId -> STM ()
track state tid =
  modifyTVar' tvar $ Set.insert tid
  where
    tvar = ccsTrackedThreads state

-- | This is used as cleanup.
--   All tracked threads should call this when they're done.
untrack :: CtrlCState -> ThreadId -> STM ()
untrack state tid =
  modifyTVar' tvar $ Set.delete tid
  where
    tvar = ccsTrackedThreads state

-- | This should run on the main thread.
--
--   @
--   main :: IO ()
--   main = withKillThese $ \state -> yourProgram
--   @
--
--   state can then be used to fork out new threads that are watched.
withKillThese :: CtrlCSettings -> (CtrlCState -> IO ()) -> IO ()
withKillThese settings fun = do
  threads <- newTVarIO mempty
  mask $ \restore -> do
    restore (fun $ MkCtrlCState
                { ccsTrackedThreads = threads
                , ccsSettings = settings
      }) `finally` do
        info StartedKilling
        killSet <- readTVarIO threads
        info $ KillingSet killSet
        -- this does not block
        traverse_ (\tid -> do
                     info $ Killing tid
                     killThread tid
                  ) killSet
        -- this does block, hence timeout
        res <- timeout (csTimeout settings) $ waitTillEmpty info threads
        case res of
          Nothing -> do
            info TimeOut
          Just _ -> do
            info ExitedGracefully

  where
    info :: LogMsg -> IO ()
    info = csLogger settings

waitTillEmpty :: (LogMsg -> IO ()) -> TVar (Set ThreadId) -> IO ()
waitTillEmpty info threads = do
    trackedThreads <- readTVarIO threads
    info $ WaitingFor trackedThreads
    unless (trackedThreads == mempty) $ do
      threadDelay 0_000_001
      waitTillEmpty info threads

newtype SignalException = SignalException Signal
  deriving (Show, Typeable)
instance Exception SignalException

-- | This provides more robust signal handling.
--   Also handles sigHup and various other signals.
installSignalHandlers :: IO ()
installSignalHandlers = do
  main_thread_id <- myThreadId
  weak_tid <- mkWeakThreadId main_thread_id
  forM_ [ sigHUP, sigTERM, sigUSR1, sigUSR2, sigXCPU, sigXFSZ ] $ \sig ->
    installHandler sig (Catch $ send_exception weak_tid sig) Nothing
  where
    send_exception weak_tid sig = do
      m <- deRefWeak weak_tid
      case m of
        Nothing  -> return ()
        Just tid -> throwTo tid (toException $ SignalException sig)
