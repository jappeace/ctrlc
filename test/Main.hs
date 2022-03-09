{-# LANGUAGE NumericUnderscores #-}

module Main where

import System.Random
import           Control.Concurrent
import           Control.Exception          (bracket)
import           Control.Monad
import           CtrlC                      (defSettings, forkTracked, csLogger,
                                             withKillThese, csTimeout , toString)
import           System.Timeout
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import Control.Concurrent.STM.TVar
import GHC.Conc(atomically)
import qualified Data.Text as Text
import System.Log.FastLogger

-- TODO we can figure out that pure loop by asking the thread status
-- with block reason
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-Conc-Sync.html#t:BlockReason

main :: IO ()
main = defaultMainWithIngredients
  defaultIngredients
  unitTests

unitTests :: TestTree
unitTests = testGroup "Thread cleanup"
  [ testCase "ForkIO doesn't cleanup" $ do
      mvar <- newEmptyMVar
      enterMvar <- newEmptyMVar
      setMvarThreadId <- forkIO $ do
            -- forking twice is the behavior we want, the thread with
            -- setMVarId is emulating the main thread
            -- so the thread we're forking out here doesn't get the async exception,
            -- thus the bracket won't run.
            void $ forkIO $ awwaitThenSet enterMvar mvar
            threadDelay testTime

      -- allow set mvar thread to be forked
      readMVar enterMvar
      killThread setMvarThreadId

      res <- timeout testTime $ readMVar mvar
      Nothing @=? res

  , testCase "Double kill is alright" $ do
      tid <- forkIO $ do
        threadDelay 0_200_000 -- 0.1 second
      threadDelay 0_100_000 -- 0.1 second
      killThread tid
      killThread tid

  -- , testCase "FREEZE " $ do

  -- the following test does not hold
  , testGroup "With ctrl c the thread should be allowed to cleanup " $ (\x ->
      testCase ("number: " <> show x) (killTest x awwaitThenSet )) <$> [0..10000] -- 100 times detects

  -- , ignoreTestBecause "This will loop forever, the exception doesn't appear to arrive" $
  -- , testCase "block me bitch"  $ minimal
  ,
    ignoreTestBecause "blocks forever, makes the test process wonky (need to kill with -9)" $
    testGroup "forever pure investigation"
      [ testCase "block mystery "  blockForever2
      , testCase "block me minimal"  blockMinimal
      , testCase "discovered case ( minified) originally found when writing tests)"  blockDiscovered
      , testCase "og block forever, With ctrl c the thread should be allowed to cleanup with pure" $
          killTest 1 $ awwaitThenSet' (pure ())
    ]
  ] -- TODO write a test for this: https://ro-che.info/articles/2014-07-30-bracket#bracket-in-non-main-threads

-- I've no idea why this blocks forever
-- I initially suspected a global state between throwTo calls
-- but now it looks like it's blocking because an unrelated pure thread
-- was forked of
blockForever2 :: IO ()
blockForever2 = do
  putStrLn "start the threads"
  tid3 <- forkIO $ do
    tid2 <- forkIO $ forever $ pure ()
    threadDelay 0_100_000 -- 0.1 second
    print tid2
    killThread tid2
  putStrLn "start the sacrfice"
  tid <- forkIO $ do
    forever $ do
      x <- randomIO
      putStrLn (x : "kill m all")
    threadDelay 10_000_000 -- 10 seconds
  print tid
  threadDelay 0_200_000 -- 0.2 second
  putStrLn "kill them all"
  killThread tid

blockMinimal :: IO ()
blockMinimal = do
  tid <- forkIO $ forever $ pure ()
  threadDelay 0_100_000 -- 0.1 second
  killThread tid


blockDiscovered :: IO ()
blockDiscovered = do
  waitVar <- newEmptyMVar
  x <- forkIO $
    bracket (pure ()) (\_ -> putMVar waitVar ()) $ forever $ pure ()
  killThread x
  takeMVar waitVar

-- I think I iddin't do the case where the main thread gets killed
-- which kills all children
killTest  :: Int -> (MVar () -> MVar Bool ->  IO ()) -> IO ()
killTest  testNr fun = do
  res <- timeout ultimateTimeout $ withFastLogger (LogFile (FileLogSpec ("killtest-" <> show testNr) 16777216 4) 1) $ \logger -> do
      -- the mvar starts as false
      mvar <- newEmptyMVar
      threadSync <- newEmptyMVar

      mainTid <- forkIO $

          withKillThese (defSettings
                          {csLogger = logger . toLogStr . Text.pack . toString}
                          -- {csTimeout = 0_200_000 }
                        ) $ \cstate -> do
              -- we track the thread
              void $ forkTracked cstate $ fun threadSync mvar
              threadDelay testTime


      logger "waiting"
      readMVar threadSync

      logger "killing main"
      killThread mainTid

      logger "reading mvar"
      res <- timeout testTime $ readMVar mvar
      assertEqual "If these aren't equal the bracket wasn't closed correctly" (Just True) res
  assertEqual "if this is false, the entire test blocked on something" (Just ()) res


testTime :: Int
testTime = 5_000_000

ultimateTimeout :: Int
ultimateTimeout = 10_000_000

awwaitThenSet' :: IO () ->
  MVar () -> -- ^ I've entered the bracket
  MVar Bool -> -- ^ I started going cleanup
  IO ()
awwaitThenSet' fun threadSync mvar =
           bracket (pure mvar) (\x -> do
              -- putStrLn "cleaning up"
              -- threadDelay setTime -- 2 seconds
              -- yield
              -- putStrLn ""
              putMVar x True
            ) (const $ do
               putMVar threadSync ()
               forever fun)

awwaitThenSet :: MVar () -> MVar Bool -> IO ()
awwaitThenSet = awwaitThenSet' (do
                                                yield
                                    )
