{-# LANGUAGE NumericUnderscores #-}
module Test.TestSpec
  ( spec
  )
where

import           CtrlC (withKillThese, forkTracked, defSettings, printLogger, csLogger )
import           Test.Hspec
import System.Timeout
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Exception (bracket)

one :: Int
one = 1

-- TODO write a test for this: https://ro-che.info/articles/2014-07-30-bracket#bracket-in-non-main-threads

setTime = 0_200_000

testTime = setTime + 0_100_00

awwaitThenSet :: MVar Bool -> IO ()
awwaitThenSet mvar =
           bracket (pure mvar) (\x -> do
              threadDelay setTime -- 2 seconds
              putMVar x True
            ) (const $ forever $ pure ())

spec :: Spec
spec =
  describe "Thread cleanup" $ do
    it "ForkIO doesn't cleanup" $ do
      putStrLn "ForkIO doesn't cleanup"
      mvar <- newEmptyMVar
      setMvarThreadId <- forkIO $ do
            -- forking twice is the behavior we want, the thread with
            -- setMVarId is emulating the main thread
            void $ forkIO $ awwaitThenSet mvar
      putStrLn "ForkIO doesn't cleanup 2"

      -- allow set mvar thread to be forked
      threadDelay 0_100_000 -- 0.1 second
      putStrLn "killing ze htread"
      killThread setMvarThreadId
      putStrLn "ForkIO doesn't cleanup 3"

      res <- timeout testTime $ takeMVar mvar
      Nothing `shouldBe` res
    it "With ctrl c the thread should be allowed to cleanup" $ do
      putStrLn "With ctrl c the thread should be allowed to cleanup"
      mvar <- newEmptyMVar
      setMvarThreadId <- forkIO $ do
          withKillThese (defSettings {csLogger = printLogger}) $ \cstate -> do
            void $ forkTracked cstate $ awwaitThenSet mvar

      -- allow set mvar thread to be forked
      threadDelay 0_100_000 -- 0.1 second
      killThread setMvarThreadId

      res <- timeout testTime $ takeMVar mvar
      Just True `shouldBe` res
