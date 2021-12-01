{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception          (bracket)
import           Control.Monad
import           CtrlC                      (csLogger, defSettings, forkTracked,
                                             printLogger, withKillThese)
import           System.Timeout
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck      as QC

import           Data.List                  (sort)

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
  , testCase "With ctrl c the thread should be allowed to cleanup" $ do
      mvar <- newEmptyMVar
      setMvarThreadId <- forkIO $ do
          withKillThese (defSettings
                         -- {csLogger = printLogger}
                        ) $ \cstate -> do
            void $ forkTracked cstate $ awwaitThenSet mvar

      -- allow set mvar thread to be forked
      threadDelay 0_100_000 -- 0.1 second
      killThread setMvarThreadId

      res <- timeout testTime $ takeMVar mvar
      Just True @=? res

  , ignoreTestBecause "This will loop forever, the exception doesn't appear to arrive" $
    testCase "With ctrl c the thread should be allowed to cleanup with pure" $ do
      mvar <- newEmptyMVar
      setMvarThreadId <- forkIO $ do
          withKillThese (defSettings
                          -- {csLogger = printLogger}
                        ) $ \cstate -> do
            void $ forkTracked cstate $ awwaitThenSet' mvar (pure ())

      -- allow set mvar thread to be forked
      threadDelay 0_100_000 -- 0.1 second
      killThread setMvarThreadId

      res <- timeout testTime $ takeMVar mvar
      Just True @=? res
  ]

-- TODO write a test for this: https://ro-che.info/articles/2014-07-30-bracket#bracket-in-non-main-threads

setTime :: Int
setTime = 0_200_000

testTime :: Int
testTime = setTime + 0_100_00

awwaitThenSet' :: MVar Bool -> IO () -> IO ()
awwaitThenSet' mvar fun =
           bracket (pure mvar) (\x -> do
              threadDelay setTime -- 2 seconds
              putMVar x True
            ) (const $ forever fun)

awwaitThenSet :: MVar Bool -> IO ()
awwaitThenSet mvar = awwaitThenSet' mvar yield

