{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- TODO write a test for this: https://ro-che.info/articles/2014-07-30-bracket#bracket-in-non-main-threads
-- TODO what if my thread spawns a thread?
-- TODO can I just kindoff fix this for forkio instead?
--      passing ctrlc around is annoying.
--      what's the main thread id anyway?
--      maybe I should just kill everything besides main..
-- | Deal with ctrl c events nicely.
--   don't just kill the main thread, kill every other registered thread as well.
--
-- + https://ro-che.info/articles/2014-07-30-bracket
-- + https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/
-- + https://simonmar.github.io/posts/2017-01-24-asynchronous-exceptions.html
-- + https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Exception.html#v:throwTo
-- + https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-Conc-Sync.html#t:BlockReason
module CtrlC
  ( withKillThese
  , CtrlCSettings(..)
  , defSettings
  , forkTracked
  , track
  , untrack
  , installSignalHandlers
  , SignalException(..)
  , defLogger
  , toString
  )
where

import GHC.Conc.Sync
import Control.Monad
import qualified Data.Set as Set
import Data.Foldable
import Control.Exception
import Data.Set(Set)
import Control.Concurrent.STM.TVar
import Control.Concurrent
import System.Timeout
import Data.Typeable (Typeable)
import System.Posix.Signals
import System.Mem.Weak (deRefWeak)

data LogMsg = ExitedGracefully
            | TimeOut
            | Killing ThreadId ThreadStatus
            | GettingStatus ThreadId
            | Tracking ThreadId
            | Untracking ThreadId
            | KillingSet (Set ThreadId)
            | WaitingFor (Set ThreadId)
            | StartedKilling
            | Killed ThreadId
            | TimedOutKilling ThreadId

defLogger :: LogMsg -> IO ()
defLogger _ = pure ()

toString :: LogMsg -> String
toString logmsg = "CtrlC: " <> msg <> "\n"
  where
    msg = case logmsg of
     ExitedGracefully -> "ExitedGracefully"
     TimeOut -> "TimeOut"
     GettingStatus tid -> "Getting status " <> show tid
     (Killing tid status) -> "Killing " <> show tid  <> " " <> show status
     (Tracking tid) -> "Tracking" <> show tid
     (Untracking tid) -> "Untracking" <> show tid
     (KillingSet tset) -> "Killing set " <> show tset
     StartedKilling -> "Started Killing"
     (WaitingFor tset) -> "Waiting for these threads to untrack themselves to indicate dying gracefully " <> show tset
     (Killed tid) -> "Killed " <> show tid
     (TimedOutKilling tid) -> "TimedOutKilling " <> show tid

data CtrlCState = MkCtrlCState {
  -- yup this isn't great, I guess v2 would have another worker thread blocking on the queue
  -- and doing the modification of cstate
    ccsTrackedThreads :: TVar (Set ThreadId) -- ^ excluding the main thread
  , ccsSettings :: CtrlCSettings
}
data CtrlCSettings = MkCtrlCSettings {
    -- | in microseconds (1/10^6 seconds),
    --   smaller then 0 means no timeout.
    --
    --   This is to prevent waiting on threads who catch and don't rethrow
    --   the threadKilled exception. (preventing us from killing them).
    --
    --   This will not work for threads who don't reach any safe points
    --   (aka garbage collection),
    --   because throwTo will block indefinetly on that,
    --   preventing the exception [from being delivered](https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Exception.html#v:throwTo).
    csTimeout :: Int
    -- | I'd recommend a logger
    --   that can deal with concurency [fastlogger](https://hackage.haskell.org/package/fast-logger-3.1.1/docs/System-Log-FastLogger.html),
    --   print is known to cause issues.
    --
    --   by default this gets ignored
  , csLogger :: LogMsg -> IO ()
  }

defSettings :: CtrlCSettings
defSettings = MkCtrlCSettings 2_000_000 defLogger

-- | this will fork out a thread that is already tracked by CtrlCState,
--   it also has the untrack handler attached.
forkTracked :: CtrlCState -> IO () -> IO ThreadId
forkTracked state io = mask $ \restore -> do
    forkIO $ do
      tid <- myThreadId
      info $ Tracking tid
      atomically $ track state tid
      restore io `finally` do
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
    info $ KillingSet trackedThreads
    -- this does not block
    traverse_ (\tid -> do -- we keep on sending exceptions
                     info $ GettingStatus tid
                     status <- threadStatus tid
                     info $ Killing tid status
                     killThread tid
                  ) trackedThreads
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
