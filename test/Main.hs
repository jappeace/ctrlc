{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Control.Concurrent
import           Control.Exception          (bracket)
import           Control.Monad
import           CtrlC                      (defSettings, forkTracked, csLogger, printLogger,
                                             withKillThese, csTimeout )
import           System.Timeout
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Thread cleanup"
  [ testCase "ForkIO doesn't cleanup" $ do
      mvar <- newEmptyMVar
      setMvarThreadId <- forkIO $ do
            -- forking twice is the behavior we want, the thread with
            -- setMVarId is emulating the main thread
            -- so the thread we're forking out here doesn't get the async exception,
            -- thus the bracket won't run.
            void $ forkIO $ awwaitThenSet mvar

      -- allow set mvar thread to be forked
      threadDelay 0_100_000 -- 0.1 second
      killThread setMvarThreadId

      res <- timeout testTime $ takeMVar mvar
      Nothing @=? res

  -- the following test does not hold
  , testGroup "With ctrl c the thread should be allowed to cleanup " $ (\x ->
      testCase ("number: " <> show x) (killTest awwaitThenSet)) <$> [0..10]

  , ignoreTestBecause "This will loop forever, the exception doesn't appear to arrive" $
    testCase "With ctrl c the thread should be allowed to cleanup with pure" $
      killTest $ awwaitThenSet' (pure ())
  ] -- TODO write a test for this: https://ro-che.info/articles/2014-07-30-bracket#bracket-in-non-main-threads

killTest  :: (MVar Bool ->  IO ()) -> IO ()
killTest  fun = do
  res <- timeout ultimateTimeout $ do
      mvar <- newEmptyMVar
      setMvarThreadId <- forkIO $ do
          withKillThese (defSettings
                          {csLogger = printLogger}
                          -- {csTimeout = 0_200_000 }
                        ) $ \cstate -> do
            void $ forkTracked cstate $ fun mvar

      -- allow set mvar thread to be forked
      threadDelay 0_100_000 -- 0.1 second
      killThread setMvarThreadId

      res <- timeout testTime $ takeMVar mvar
      Just True @=? res
  assertEqual "if this is false, the entire test blocked on something" (Just ()) res


setTime :: Int
setTime = 0_200_000

testTime :: Int
testTime = setTime + setTime

ultimateTimeout :: Int
ultimateTimeout = 1_000_000

awwaitThenSet' :: IO () -> MVar Bool ->  IO ()
awwaitThenSet' fun mvar =
           bracket (pure mvar) (\x -> do
              threadDelay setTime -- 2 seconds
              putMVar x True
            ) (const $ forever fun)

awwaitThenSet :: MVar Bool -> IO ()
awwaitThenSet mvar = awwaitThenSet' yield mvar
