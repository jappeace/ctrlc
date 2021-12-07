-- TODO write a test for this: https://ro-che.info/articles/2014-07-30-bracket#bracket-in-non-main-threads
-- | Deal with ctrl c events nicely.
--   don't just kill the main thread, kill every other registered thread as well.
-- https://ro-che.info/articles/2014-07-30-bracket
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

import qualified Data.Map as Map
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
import Control.Concurrent.Async(Async,async, wait, asyncThreadId, cancel)
import Control.Concurrent.STM.TQueue
import Data.Monoid (Endo(..), appEndo)

data LogMsg = ExitedGracefully
            | TimeOut
            | Killing ThreadId
            | KillingSet (Set ThreadId)
            | WaitingFor (Set ThreadId)
            | StartedKilling

defLogger :: LogMsg -> IO ()
defLogger _ = pure ()

toString :: LogMsg -> String
toString ExitedGracefully = "CtrlC: ExitedGracefully"
toString TimeOut = "CtrlC: TimeOut"
toString (Killing tid) = "CtrlC: Killing " <> show tid
toString (KillingSet tset) = "CtrlC: Killing set " <> show tset
toString StartedKilling = "CtrlC: Started Killing"
toString (WaitingFor tset) = "CtrlC: Waiting for these threads to untrack themselves to indicate dying gracefully " <> show tset

printLogger :: LogMsg -> IO ()
printLogger = putStrLn . toString

data CtrlCState = MkCtrlCState {
  -- yup this isn't great, I guess v2 would have another worker thread blocking on the queue
  -- and doing the modification of cstate
    ccsTrackedThreads :: TQueue (Map ThreadId (Async ()) -> Map ThreadId (Async ())) -- excluding the main thread
}
data CtrlCSettings = MkCtrlCSettings {
    csTimeout :: Int -- ^ in microseconds (1/10^6 seconds), prevents infinite blocking, smaller then 0 means no timeout
  , csLogger :: LogMsg -> IO ()
  }

defSettings :: CtrlCSettings
defSettings = MkCtrlCSettings 2000000 defLogger

-- | this will fork out a thread that is already tracked by CtrlCState,
--   it also has the untrack handler attached.
forkTracked :: CtrlCState -> IO () -> IO (Async ())
forkTracked state io = do
  mask $ \restore -> do -- if you want to fork..
    tid <- restore $ async $ restore io
    restore $ forkIO $ -- this allows us to provide Async to the user, while we also wait on it
                wait tid `finally` atomically (untrack state $ asyncThreadId tid)
    atomically $ track state tid -- but we need to not except here
    pure tid

-- | Starts tracking a thread, we expect this thread to untrack itself
--   to indicate it is done with cleaning up.
track :: CtrlCState -> Async () -> STM ()
track state tid =
  writeTQueue tvar (Map.insert (asyncThreadId tid) tid)
  where
    tvar = ccsTrackedThreads state

-- | This is used as cleanup.
--   All tracked threads should call this when they're done.
untrack :: CtrlCState -> ThreadId -> STM ()
untrack state tid =
  writeTQueue tvar (Map.delete tid)
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
  threads <- newTQueueIO
  mask $ \restore -> do
    restore (fun $ MkCtrlCState
                { ccsTrackedThreads = threads
      }) `finally` do
        info StartedKilling
        opList <- atomically $ flushTQueue threads
        let threadMap :: Map ThreadId (Async ())
            threadMap  = appEndo (foldMap Endo opList) mempty
        info $ KillingSet $ Map.keysSet threadMap
        -- this does not block
        traverse_ (\asyncH -> do
                     let tid = asyncThreadId asyncH
                     info $ Killing tid
                     killThread tid
                  ) threadMap
        -- this does block, hence timeout
        res <- timeout (csTimeout settings) $ traverse_ cancel threadMap
        case res of
          Nothing -> do
            info TimeOut
          Just _ -> do
            info ExitedGracefully

  where
    info :: LogMsg -> IO ()
    info = csLogger settings


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
